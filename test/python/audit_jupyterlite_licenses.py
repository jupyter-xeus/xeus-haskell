#!/usr/bin/env python3
"""Verify that JupyterLite publishes notices and a matching source bundle."""

from __future__ import annotations

import hashlib
import sys
import tarfile
from pathlib import Path

EXPECTED_MEMBERS = {
    "usr/share/licenses/xeus-haskell/SOURCE_OFFER.md",
    "usr/share/licenses/xeus-haskell/licenses.toml",
    "usr/share/licenses/xeus-haskell/THIRD_PARTY_NOTICES.md",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def normalized_members(archive: Path) -> set[str]:
    with tarfile.open(archive, "r:gz") as bundle:
        return {member.name.removeprefix("./").lstrip("/") for member in bundle}


def find_complete_mount(site: Path) -> Path:
    for archive in sorted(site.rglob("mount_*.tar.gz")):
        if EXPECTED_MEMBERS <= normalized_members(archive):
            return archive
    raise RuntimeError("no Xeus mount contains the complete license notice tree")


def verify_source_bundle(site: Path, offer: str) -> Path:
    bundles = sorted(site.glob("corresponding-source/*/*.tar.zst"))
    if len(bundles) != 1:
        raise RuntimeError(f"expected one corresponding-source bundle, found {len(bundles)}")
    bundle = bundles[0]
    checksum = bundle.with_suffix(bundle.suffix + ".sha256")
    if not checksum.is_file():
        raise RuntimeError(f"missing checksum for {bundle}")
    expected = checksum.read_text().split()[0]
    if sha256(bundle) != expected:
        raise RuntimeError(f"checksum mismatch for {bundle}")
    public_path = "/" + bundle.relative_to(site).as_posix()
    if bundle.name not in offer or public_path not in offer or expected not in offer:
        raise RuntimeError("source offer does not identify the deployed bundle and checksum")
    return bundle


def main() -> int:
    site = Path(sys.argv[1] if len(sys.argv) > 1 else "dist")
    if not site.is_dir():
        raise RuntimeError(f"JupyterLite output directory does not exist: {site}")
    mount = find_complete_mount(site)
    with tarfile.open(mount, "r:gz") as bundle:
        offer_member = bundle.extractfile("usr/share/licenses/xeus-haskell/SOURCE_OFFER.md")
        assert offer_member is not None
        offer = offer_member.read().decode()
    source = verify_source_bundle(site, offer)
    print(f"verified JupyterLite notices and corresponding source: {source}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
