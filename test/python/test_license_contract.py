from __future__ import annotations

import hashlib
import importlib.util
import json
import tomllib
import subprocess
import tarfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LICENSES_MANIFEST = ROOT / "licenses.toml"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def test_ghc_resource_scripts_parse() -> None:
    scripts = sorted((ROOT / "xhaskell" / "ghc" / "scripts").glob("*.sh"))
    subprocess.run(["bash", "-n", *scripts], cwd=ROOT, check=True)


def test_license_texts_are_not_tracked() -> None:
    result = subprocess.run(
        ["git", "ls-files", "--", "xhaskell/microhs/licenses", "xhaskell/ghc/licenses"],
        cwd=ROOT, check=True, capture_output=True, text=True,
    )
    assert not result.stdout.splitlines()


def test_license_manifest_pins_native_bignum_ghc_and_build_assets() -> None:
    manifest = tomllib.loads(LICENSES_MANIFEST.read_text())
    assert manifest["ghc"]["repository"] == "https://github.com/haskell-wasm/ghc.git"
    assert manifest["ghc"]["commit"] == "05e0ef08e100cf3bd2150adcf8e53ba9ad30519a"
    assert len(manifest["ghc"]["submodules"]) == 33
    assert all("gmp" not in entry["path"] for entry in manifest["ghc"]["submodules"])
    assert "gmp" not in manifest
    wasm = manifest["ghc"]["wasm"]
    assert wasm["version"] == "9.15.20260331"
    assert wasm["toolchain"]["flavour"] == "native"
    assert len(wasm["toolchain"]["sha256"]) == 64
    assert all(len(asset["sha256"]) == 64 and asset["url"].startswith("https://")
               for asset in wasm["browser_assets"])
    assert all(entry["url"].startswith("https://") and len(entry["sha256"]) == 64
               for entry in manifest["archives"])


def test_corresponding_source_initializes_top_level_submodules() -> None:
    script = ROOT / "tools" / "build_corresponding_source.py"
    spec = importlib.util.spec_from_file_location("corresponding_source", script)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    manifest = tomllib.loads(LICENSES_MANIFEST.read_text())
    paths = module.checkout_submodule_paths(manifest)
    assert "libraries/containers" in paths
    assert "libraries/containers/containers/docs/_extensions/haddock-autolink" not in paths


def test_license_metadata_tool_rejects_gmp_and_emits_native_resources(tmp_path: Path) -> None:
    output = tmp_path / "resources.json"
    subprocess.run(
        ["python", "tools/licenses.py", "ghc-resources", "--manifest", str(LICENSES_MANIFEST),
         "--output", str(output)],
        cwd=ROOT,
        check=True,
    )
    resources = json.loads(output.read_text())
    assert resources["toolchain"]["flavour"] == "native"
    assert resources["ghc_version"] == "9.15.20260331"

    invalid_manifest = tmp_path / "licenses.toml"
    invalid_manifest.write_text(LICENSES_MANIFEST.read_text() + "\n[gmp]\nname = 'GMP'\n")
    rejected = subprocess.run(
        ["python", "tools/licenses.py", "ghc-version", "--manifest", str(invalid_manifest)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert rejected.returncode != 0
    assert "must not declare GMP" in rejected.stderr


def test_microhs_gmp_is_disabled_and_license_collection_is_generated() -> None:
    microhs_cmake = (ROOT / "cmake" / "MicroHs.cmake").read_text()
    manifest = tomllib.loads(LICENSES_MANIFEST.read_text())
    assert "WANT_GMP" not in microhs_cmake
    assert "MICROHS_GMP" not in microhs_cmake
    assert 'PATTERN "lib/gmp" EXCLUDE' in microhs_cmake
    assert all(not entry["id"].startswith("microhs-gmp-") for entry in manifest["archives"])
    builder = (ROOT / "tools" / "build_corresponding_source.py").read_text()
    assert "collect_licenses" in builder
    assert "not stored in the xeus-haskell Git repository" in builder
    assert "must not fetch GMP" in builder


def test_pixi_and_ci_target_linux_amd64_only() -> None:
    pixi = tomllib.loads((ROOT / "pixi.toml").read_text())
    assert pixi["workspace"]["platforms"] == ["linux-64"]
    assert pixi["feature"]["native"]["platforms"] == ["linux-64"]
    assert pixi["feature"]["wasm-build"]["platforms"] == ["linux-64"]
    assert pixi["feature"]["browser-test"]["platforms"] == ["linux-64"]
    workflow = (ROOT / ".github" / "workflows" / "main.yml").read_text()
    assert "runs-on: ubuntu-latest" in workflow
    assert "macos-latest" not in workflow
    assert "windows-latest" not in workflow


def test_jupyterlite_auditor_accepts_matching_offer(tmp_path: Path) -> None:
    script = ROOT / "test" / "python" / "audit_jupyterlite_licenses.py"
    spec = importlib.util.spec_from_file_location("license_audit", script)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    site = tmp_path / "dist"
    payload = tmp_path / "payload"
    bundle = site / "corresponding-source" / "test" / "xeus-haskell-test-corresponding-source.tar.zst"
    bundle.parent.mkdir(parents=True)
    bundle.write_bytes(b"source-bundle")
    digest = sha256(bundle)
    bundle.with_suffix(bundle.suffix + ".sha256").write_text(f"{digest}  {bundle.name}\n")
    offer = f"URL: /corresponding-source/test/{bundle.name}\nSHA-256: {digest}\n"
    for member in module.EXPECTED_MEMBERS:
        path = payload / member
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(offer if path.name == "SOURCE_OFFER.md" else "generated-license")
    archive = site / "packages" / "mount_2.tar.gz"
    archive.parent.mkdir(parents=True, exist_ok=True)
    with tarfile.open(archive, "w:gz") as mounted:
        mounted.add(payload / "usr", arcname="usr")
    with tarfile.open(module.find_complete_mount(site), "r:gz") as mounted:
        offer_member = mounted.extractfile("usr/share/licenses/xeus-haskell/SOURCE_OFFER.md")
        assert offer_member is not None
        assert module.verify_source_bundle(site, offer_member.read().decode()) == bundle
