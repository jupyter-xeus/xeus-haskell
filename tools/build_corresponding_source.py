#!/usr/bin/env python3
"""Build a verified corresponding-source bundle outside the Git worktree."""
from __future__ import annotations

import argparse
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import tarfile
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = ROOT / "licenses.toml"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def run(*command: str, cwd: Path | None = None) -> str:
    return subprocess.run(command, cwd=cwd, check=True, text=True,
                          stdout=subprocess.PIPE).stdout


def fetch_archive(entry: dict[str, str], cache: Path) -> Path:
    destination = cache / "archives" / entry["sha256"]
    destination.parent.mkdir(parents=True, exist_ok=True)
    if destination.exists() and sha256(destination) != entry["sha256"]:
        destination.unlink()
    if not destination.exists():
        print(f"downloading {entry['id']}: {entry['url']}", file=sys.stderr)
        partial = destination.with_suffix(".partial")
        for attempt in range(12):
            try:
                run("curl", "--fail", "--location", "--continue-at", "-", "--output", str(partial), entry["url"])
                partial.replace(destination)
                break
            except subprocess.CalledProcessError:
                if attempt == 11:
                    # Keep a partial response: a later invocation can resume it.
                    raise
                print(f"retrying {entry['id']} from the partial download", file=sys.stderr)
    actual = sha256(destination)
    if actual != entry["sha256"]:
        destination.unlink(missing_ok=True)
        raise RuntimeError(f"checksum mismatch for {entry['id']}: {actual}")
    return destination


def expected_submodules(manifest: dict) -> set[tuple[str, str]]:
    submodules = {(entry["commit"], entry["path"]) for entry in manifest["ghc"]["submodules"]}
    if any("gmp" in path.lower() for _, path in submodules):
        raise ValueError("the no-GMP corresponding-source bundle must not fetch GMP")
    return submodules


def checkout_submodule_paths(manifest: dict) -> list[str]:
    paths = [entry["path"] for entry in manifest["ghc"]["submodules"]]
    return [path for path in paths if not any(
        path.startswith(f"{parent}/") for parent in paths
    )]


def ghc_checkout(manifest: dict, cache: Path) -> Path:
    details = manifest["ghc"]
    repository_key = hashlib.sha256(details["repository"].encode()).hexdigest()[:16]
    checkout = cache / "ghc" / repository_key / details["commit"]
    if not checkout.exists():
        checkout.parent.mkdir(parents=True, exist_ok=True)
        print(f"cloning GHC from {details['repository']}", file=sys.stderr)
        run("git", "clone", "--no-checkout", details["repository"], str(checkout))
    # The pinned commit can be outside the default branch, so fetch it explicitly.
    run("git", "fetch", "--depth=1", "origin", details["commit"], cwd=checkout)
    run("git", "checkout", "--detach", "FETCH_HEAD", cwd=checkout)
    run("git", "submodule", "sync", "--recursive", cwd=checkout)
    submodules = checkout_submodule_paths(manifest)
    run("git", "-c", "submodule.fetchJobs=8", "submodule", "update", "--init", "--recursive", "--", *submodules, cwd=checkout)
    if run("git", "rev-parse", "HEAD", cwd=checkout).strip() != details["commit"]:
        raise RuntimeError("cached GHC checkout is at the wrong revision")
    expected = expected_submodules(manifest)
    expected_paths = {path for _, path in expected}
    actual = set()
    for line in run("git", "submodule", "status", "--recursive", cwd=checkout).splitlines():
        if not line:
            continue
        fields = line.lstrip("-+U ").split()
        if fields[1] in expected_paths:
            actual.add((fields[0], fields[1]))
    if actual != expected:
        raise RuntimeError("GHC recursive submodule lock does not match")
    return checkout


def copy_tree(source: Path, destination: Path) -> None:
    shutil.copytree(source, destination, symlinks=True, dirs_exist_ok=True, ignore=shutil.ignore_patterns(".git"))


def is_license_name(name: str) -> bool:
    upper = Path(name).name.upper()
    return upper.startswith(("LICENSE", "COPYING", "COPYRIGHT", "NOTICE"))


def collect_licenses(staging: Path, output: Path) -> None:
    """Collect distributable license texts from the pinned non-GMP source inputs."""
    output.mkdir(parents=True, exist_ok=True)
    for source in (staging / "ghc").rglob("*"):
        if source.is_file() and is_license_name(source.name):
            relative = source.relative_to(staging / "ghc")
            target = output / "ghc" / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
    for archive in staging.rglob("*.tar.*"):
        with tarfile.open(archive) as contents:
            for member in contents.getmembers():
                if member.isfile() and is_license_name(member.name):
                    target = output / "archives" / archive.relative_to(staging) / member.name
                    target.parent.mkdir(parents=True, exist_ok=True)
                    with contents.extractfile(member) as source, target.open("wb") as destination:
                        shutil.copyfileobj(source, destination)
    (output / "THIRD_PARTY_NOTICES.md").write_text(
        "# Generated third-party license collection\n\n"
        "This directory is generated from the pinned corresponding-source inputs; it is not stored in the xeus-haskell Git repository.\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cache-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--distribution-id",
                        help="distribution identifier (defaults to the current Git revision)")
    parser.add_argument("--public-url",
                        help="public directory containing the bundle")
    parser.add_argument("--license-output-dir", type=Path,
                        help="directory receiving generated notices and SOURCE_OFFER.md")
    parser.add_argument("--publish-dir", type=Path,
                        help="directory receiving the bundle and checksum for publication")
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    args = parser.parse_args()

    distribution_id = args.distribution_id or run("git", "rev-parse", "--short=12", "HEAD", cwd=ROOT).strip()
    public_url = args.public_url or f"/corresponding-source/{distribution_id}"
    manifest = tomllib.loads(args.manifest.read_text())
    args.cache_dir.mkdir(parents=True, exist_ok=True)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    bundle_name = f"xeus-haskell-{distribution_id}-corresponding-source.tar.zst"
    bundle = args.output_dir / bundle_name

    with tempfile.TemporaryDirectory(prefix="xeus-haskell-source-") as temporary:
        staging = Path(temporary) / f"xeus-haskell-{distribution_id}-corresponding-source"
        staging.mkdir()
        shutil.copy2(args.manifest, staging / "licenses.toml")
        for entry in manifest["archives"]:
            source = fetch_archive(entry, args.cache_dir)
            target = staging / entry["destination"]
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
        checkout = ghc_checkout(manifest, args.cache_dir)
        copy_tree(checkout, staging / "ghc" / f"ghc-{manifest['ghc']['commit']}")
        collect_licenses(staging, args.output_dir / "licenses")
        run("tar", "--sort=name", "--mtime=@0", "--owner=0", "--group=0",
            "--numeric-owner", "--zstd", "-cf", str(bundle), "-C", str(staging.parent), staging.name)

    digest = sha256(bundle)
    checksum = bundle.with_suffix(bundle.suffix + ".sha256")
    checksum.write_text(f"{digest}  {bundle.name}\n")
    public_url = public_url.rstrip("/")
    offer = args.output_dir / "SOURCE_OFFER.md"
    offer.write_text(
        "# Corresponding source offer\n\n"
        f"This xeus-haskell distribution is accompanied by `{bundle.name}`.\n\n"
        f"URL: `{public_url}/{bundle.name}`\n\n"
        f"SHA-256: `{digest}`\n\n"
        "The bundle contains the exact native-bignum GHC checkout with the "
        "required recursive submodules, plus the libzmq source and conda-forge recipe.\n"
    )
    license_output_dir = args.license_output_dir
    if license_output_dir is None and os.environ.get("XHASKELL_SOURCE_LICENSE_DIR"):
        license_output_dir = Path(os.environ["XHASKELL_SOURCE_LICENSE_DIR"])
    if license_output_dir:
        license_output_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(offer, license_output_dir / offer.name)
        shutil.copytree(args.output_dir / "licenses", license_output_dir, dirs_exist_ok=True)
    if args.publish_dir:
        args.publish_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(bundle, args.publish_dir / bundle.name)
        shutil.copy2(checksum, args.publish_dir / checksum.name)
    print(bundle)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
