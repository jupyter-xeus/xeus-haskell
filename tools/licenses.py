#!/usr/bin/env python3
"""Read the central external-source and license provenance manifest."""
from __future__ import annotations

import argparse
import json
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = ROOT / "licenses.toml"


def read_manifest(path: Path) -> dict:
    manifest = tomllib.loads(path.read_text())
    wasm = manifest["ghc"]["wasm"]
    if wasm["toolchain"]["flavour"] != "native":
        raise ValueError("the GHC Wasm toolchain must use the native bignum flavour")
    if "gmp" in manifest or any(
        "gmp" in entry["path"].lower() for entry in manifest["ghc"]["submodules"]
    ):
        raise ValueError("the no-GMP distribution must not declare GMP source inputs")
    return manifest


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=("ghc-version", "ghc-resources"))
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    manifest = read_manifest(args.manifest)

    if args.command == "ghc-version":
        if args.output:
            parser.error("--output is only valid with ghc-resources")
        print(manifest["ghc"]["wasm"]["version"])
        return 0

    if args.output is None:
        parser.error("ghc-resources requires --output")
    wasm = manifest["ghc"]["wasm"]
    resource_lock = {
        "ghc_version": wasm["version"],
        "toolchain": wasm["toolchain"],
        "browser_assets": wasm["browser_assets"],
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(resource_lock, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
