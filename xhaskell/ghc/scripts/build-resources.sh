#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
ghc_dir=$(cd -- "$script_dir/.." && pwd)
project_dir=$(cd -- "$ghc_dir/../.." && pwd)

die() {
  echo "ghc-wasm-resources: $*" >&2
  exit 1
}

usage() {
  echo "Usage: build-resources.sh --output DIR --build-dir DIR"
}

output_dir=""
build_dir=""
while [[ $# -gt 0 ]]; do
  case $1 in
    --output) [[ $# -ge 2 ]] || die "--output requires a value"; output_dir=$2; shift 2 ;;
    --build-dir) [[ $# -ge 2 ]] || die "--build-dir requires a value"; build_dir=$2; shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) die "unknown build option: $1" ;;
  esac
done

[[ -n $output_dir ]] || die "--output is required"
[[ -n $build_dir ]] || die "--build-dir is required"

output_dir=$(realpath -m "$output_dir")
build_dir=$(realpath -m "$build_dir")
mkdir -p "$build_dir"

resource_lock="$build_dir/resources.lock.json"
python3 "$project_dir/tools/licenses.py" ghc-resources \
  --manifest "$project_dir/licenses.toml" \
  --output "$resource_lock"

toolchain_revision=$(jq -er '.toolchain.revision' "$resource_lock")
toolchain_flavour=$(jq -er '.toolchain.flavour' "$resource_lock")
toolchain_digest=$(jq -er '.toolchain.sha256' "$resource_lock")
toolchain_key="${toolchain_revision:0:12}-${toolchain_flavour}-${toolchain_digest:0:12}"
toolchain_dir="$build_dir/toolchains/$toolchain_key"
vendor_dir="$build_dir/browser-assets"

"$script_dir/prepare-inputs.sh" \
  "$resource_lock" "$build_dir" "$toolchain_dir" "$vendor_dir"

# ghc-wasm-meta emits the compiler paths and runtime environment here.
# shellcheck disable=SC1090
source "$toolchain_dir/env"

work_dir=$(mktemp -d "$build_dir/resources.XXXXXXXX")
trap 'rm -rf -- "$work_dir"' EXIT
runtime_dir="$work_dir/runtime"
staged_output="$work_dir/output"

"$script_dir/build-runtime.sh" \
  "$ghc_dir" "$work_dir" "$runtime_dir" \
  "$(jq -er '.ghc_version' "$resource_lock")"
"$script_dir/assemble-rootfs.sh" \
  "$runtime_dir" "$vendor_dir" "$work_dir" "$staged_output"

mkdir -p "$output_dir"
for resource in "$staged_output"/*; do
  mv -f -- "$resource" "$output_dir/"
done

echo "GHC JavaScript-Wasm resources written to $output_dir"
