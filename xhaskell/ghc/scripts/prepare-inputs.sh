#!/usr/bin/env bash

set -euo pipefail

[[ $# -eq 4 ]] || {
  echo "Usage: prepare-inputs.sh LOCK_FILE BUILD_DIR TOOLCHAIN_DIR VENDOR_DIR" >&2
  exit 2
}

resource_lock=$1
build_dir=$2
toolchain_dir=$3
vendor_dir=$4

die() {
  echo "ghc-wasm-inputs: $*" >&2
  exit 1
}

verify_sha256() {
  local actual
  actual=$(sha256sum "$1" | awk '{print $1}')
  [[ $actual == "$2" ]] || die "SHA-256 mismatch for $1: expected $2, found $actual"
}

fetch_file() {
  local destination=$1
  local url=$2
  local expected_sha256=$3
  local temporary_file

  if [[ -f $destination ]] && [[ $(sha256sum "$destination" | awk '{print $1}') == "$expected_sha256" ]]; then
    return
  fi

  mkdir -p "$(dirname -- "$destination")"
  temporary_file=$(mktemp "${destination}.download.XXXXXX")
  trap 'rm -f -- "$temporary_file"' RETURN
  curl --proto '=https' --tlsv1.2 --fail --silent --show-error --location \
    --output "$temporary_file" "$url"
  verify_sha256 "$temporary_file" "$expected_sha256"
  mv -f -- "$temporary_file" "$destination"
  trap - RETURN
}

install_toolchain() {
  local revision=$1
  local flavour=$2
  local archive_url=$3
  local archive_sha256=$4
  local archive_path source_dir staging_dir cc_for_build

  [[ ! -f $toolchain_dir/env ]] || return
  [[ ! -e $toolchain_dir ]] || die "incomplete toolchain at $toolchain_dir"

  archive_path="$build_dir/downloads/ghc-wasm-meta-$revision.tar.gz"
  fetch_file "$archive_path" "$archive_url" "$archive_sha256"

  mkdir -p "$(dirname -- "$toolchain_dir")"
  staging_dir=$(mktemp -d "$build_dir/toolchain.XXXXXXXX")
  trap 'rm -rf -- "$staging_dir"' RETURN
  source_dir="$staging_dir/ghc-wasm-meta"
  mkdir -p "$source_dir"
  tar -xzf "$archive_path" --directory "$source_dir" --strip-components=1

  cc_for_build=$(command -v "${CC_FOR_BUILD:-${CC:-cc}}") || \
    die "native C compiler not found: ${CC_FOR_BUILD:-${CC:-cc}}"
  cc_for_build=$(realpath "$cc_for_build")

  # ghc-wasm-meta hardcodes CC_FOR_BUILD=cc after prepending wasi-sdk/bin.
  # Keep its build tools on the native compiler selected by Pixi.
  sed -i \
    's@"CC_FOR_BUILD=cc"@"CC_FOR_BUILD=${XHASKELL_CC_FOR_BUILD:-cc}"@' \
    "$source_dir/setup.sh"
  (
    cd "$source_dir"
    env -u CFLAGS -u CPPFLAGS -u CXXFLAGS -u LDFLAGS \
      -u UPSTREAM_GHC_PIPELINE_ID -u UPSTREAM_WASI_SDK_PIPELINE_ID \
      XHASKELL_CC_FOR_BUILD="$cc_for_build" \
      PREFIX="$toolchain_dir" FLAVOUR="$flavour" bash ./setup.sh
  )

  [[ -f $toolchain_dir/env ]] || die "ghc-wasm-meta setup did not produce an environment"
  trap - RETURN
  rm -rf -- "$staging_dir"
}

install_toolchain \
  "$(jq -er '.toolchain.revision' "$resource_lock")" \
  "$(jq -er '.toolchain.flavour' "$resource_lock")" \
  "$(jq -er '.toolchain.archive_url' "$resource_lock")" \
  "$(jq -er '.toolchain.sha256' "$resource_lock")"

while IFS=$'\t' read -r relative_path url digest; do
  fetch_file "$vendor_dir/$relative_path" "$url" "$digest"
done < <(jq -r '.browser_assets[] | [.path, .url, .sha256] | @tsv' "$resource_lock")
