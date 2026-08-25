#!/usr/bin/env bash

set -euo pipefail

[[ $# -eq 4 ]] || {
  echo "Usage: build-runtime.sh GHC_SOURCE_DIR WORK_DIR RUNTIME_DIR GHC_VERSION" >&2
  exit 2
}

ghc_dir=$1
work_dir=$2
runtime_dir=$3
expected_ghc_version=$4
ghc=${WASM_GHC:-wasm32-wasi-ghc}
ghc_pkg=${WASM_GHC_PKG:-wasm32-wasi-ghc-pkg}

actual_ghc_version=$($ghc --numeric-version)
if [[ $actual_ghc_version != "$expected_ghc_version" ]]; then
  echo "ghc-wasm-runtime: expected GHC $expected_ghc_version, found $actual_ghc_version" >&2
  exit 1
fi

display_version=0.1.0
display_unit_id="xhaskell-display-$display_version"
display_dir="$runtime_dir/display"
display_import_dir="$display_dir/import"
display_library="libHS${display_unit_id}-ghc${expected_ghc_version}.so"
mkdir -p "$display_import_dir" "$work_dir/stubs"

export WASM_SO_OPT=${WASM_SO_OPT:---debuginfo --low-memory-unused --strip-dwarf -Oz}

"$ghc" \
  -v0 \
  -dynamic -fPIC \
  -this-unit-id "$display_unit_id" \
  -hisuf dyn_hi -osuf dyn_o \
  -c "$ghc_dir/haskell/XHaskell/Display.hs" \
  -odir "$display_import_dir" \
  -hidir "$display_import_dir"
"$ghc" \
  -v0 \
  -dynamic -shared -fPIC \
  -this-unit-id "$display_unit_id" \
  "$display_import_dir/XHaskell/Display.dyn_o" \
  -o "$display_dir/$display_library"

(
  cd "$work_dir"
  "$ghc" \
    -v0 \
    -package ghc \
    -shared -dynamic \
    -no-keep-hi-files -no-keep-o-files \
    -stubdir "$work_dir/stubs" \
    -O2 \
    "$ghc_dir/haskell/Playground.hs" \
    -o "$runtime_dir/libxeus-haskell-ghc.so"
)

"$ghc_pkg" field base id --simple-output > "$runtime_dir/base-unit-id"
