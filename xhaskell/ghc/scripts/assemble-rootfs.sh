#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

[[ $# -eq 4 ]] || {
  echo "Usage: assemble-rootfs.sh RUNTIME_DIR VENDOR_DIR WORK_DIR OUTPUT_DIR" >&2
  exit 2
}

runtime_dir=$1
vendor_dir=$2
work_dir=$3
output_dir=$4
ghc=${WASM_GHC:-wasm32-wasi-ghc}
ghc_pkg=${WASM_GHC_PKG:-wasm32-wasi-ghc-pkg}
cc=${WASM_CC:-wasm32-wasi-clang}

die() {
  echo "ghc-wasm-rootfs: $*" >&2
  exit 1
}

if ! tar --version 2>/dev/null | head -n 1 | grep -q 'GNU tar'; then
  die "GNU tar is required for reproducible archives"
fi

root_dir="$work_dir/root"
root_tmp="$root_dir/tmp"
ghc_libdir=$($ghc --print-libdir)
cc_path=$(realpath "$(command -v "$cc")")
wasi_libdir=$(cd -- "$(dirname -- "$cc_path")/../share/wasi-sysroot/lib/wasm32-wasi" && pwd)
mkdir -p "$root_tmp/clib" "$root_tmp/hslib" "$output_dir"

install -m 0755 "$runtime_dir/libxeus-haskell-ghc.so" "$root_tmp/libxeus-haskell-ghc.so"
cp -LR "$wasi_libdir/." "$root_tmp/clib/"
find "$root_tmp/clib" -type f ! -name '*.so' -delete
rm -f "$root_tmp/clib/libsetjmp.so" "$root_tmp/clib"/libwasi-emulated-*.so

cp -LR "$ghc_libdir" "$root_tmp/hslib/lib"
chmod -R u+w "$root_tmp/hslib/lib"
package_db="$root_tmp/hslib/lib/package.conf.d"
"$ghc_pkg" --no-user-package-db --global-package-db="$package_db" recache

for package_name in Cabal Cabal-syntax integer-gmp; do
  if [[ -n $($ghc_pkg --no-user-package-db --global-package-db="$package_db" list "$package_name" --simple-output) ]]; then
    "$ghc_pkg" --no-user-package-db --global-package-db="$package_db" unregister "$package_name"
  fi
done

find "$root_tmp/hslib/lib" \( \
  -name '*.hi' -o \
  -name '*.p_hi' -o \
  -name '*.p_dyn_hi' -o \
  -name '*.a' -o \
  -name 'libHS*_p*.so' -o \
  -name 'libHSrts*_debug*.so' \
  \) -delete
rm -rf \
  "$root_tmp/hslib/lib/doc" \
  "$root_tmp/hslib/lib/html" \
  "$root_tmp/hslib/lib/latex"
rm -f "$root_tmp/hslib/lib"/*.mjs "$root_tmp/hslib/lib"/*.js "$root_tmp/hslib/lib"/*.txt

mapfile -d '' dynamic_dirs < <(
  find "$root_tmp/hslib/lib" -type f -name '*.so' -printf '%h\0' | sort -zu
)
if [[ ${#dynamic_dirs[@]} -ne 1 ]]; then
  die "expected one GHC dynamic-library directory, found ${#dynamic_dirs[@]}"
fi

dynamic_dir=${dynamic_dirs[0]}
stable_dynamic_dir="$root_tmp/hslib/lib/wasm32-wasi-ghc"
if [[ $dynamic_dir != "$stable_dynamic_dir" ]]; then
  dynamic_dir_name=$(basename -- "$dynamic_dir")
  mv "$dynamic_dir" "$stable_dynamic_dir"
  find "$package_db" -type f -name '*.conf' -exec \
    sed -i "s|/lib/$dynamic_dir_name|/lib/wasm32-wasi-ghc|g" {} +
fi

find "$package_db" -type f -name '*.conf.copy' -delete
rm -rf "$stable_dynamic_dir"/*Cabal*
find "$root_tmp/hslib/lib" -depth -iname '*integer-gmp*' -exec rm -rf -- {} +

display_version=0.1.0
display_unit_id="xhaskell-display-$display_version"
display_root_dir="$root_tmp/hslib/lib/$display_unit_id"
display_library=("$runtime_dir/display"/libHS"$display_unit_id"-ghc*.so)
[[ ${#display_library[@]} -eq 1 ]] || die "expected one XHaskell.Display library"
display_library_name=$(basename -- "${display_library[0]}")
mkdir -p "$display_root_dir/XHaskell"
install -m 0644 \
  "$runtime_dir/display/import/XHaskell/Display.dyn_hi" \
  "$display_root_dir/XHaskell/Display.dyn_hi"
install -m 0755 "${display_library[0]}" "$stable_dynamic_dir/"

display_package_conf="$work_dir/xhaskell-display.conf"
cat > "$display_package_conf" <<EOF
name: xhaskell-display
version: $display_version
id: $display_unit_id
key: $display_unit_id
exposed: True
exposed-modules: XHaskell.Display
import-dirs: $display_root_dir
library-dirs: $stable_dynamic_dir
dynamic-library-dirs: $stable_dynamic_dir
hs-libraries: HS$display_unit_id
depends: $(<"$runtime_dir/base-unit-id")
EOF
"$ghc_pkg" --no-user-package-db --global-package-db="$package_db" register "$display_package_conf"
mapfile -t display_registered_confs < <(
  grep -l "^id:[[:space:]]*$display_unit_id$" "$package_db"/*.conf
)
[[ ${#display_registered_confs[@]} -eq 1 ]] || die "expected one registered $display_unit_id package config"
sed -i "s|$root_tmp/hslib|/tmp/hslib|g" "${display_registered_confs[0]}"
"$ghc_pkg" --no-user-package-db --global-package-db="$package_db" recache

gmp_artifact=$(find "$root_tmp" -iname '*gmp*' -print -quit)
[[ -z $gmp_artifact ]] || die "native-bignum rootfs contains a GMP artifact: $gmp_artifact"
gmp_reference=$(grep -Ril 'gmp' "$package_db" | sed -n '1p' || true)
[[ -z $gmp_reference ]] || die "native-bignum package database references GMP: $gmp_reference"

install -m 0755 "$ghc_libdir/dyld.mjs" "$output_dir/dyld.mjs"
install -m 0755 "$ghc_libdir/post-link.mjs" "$output_dir/post-link.mjs"
install -m 0644 "$ghc_libdir/prelude.mjs" "$output_dir/prelude.mjs"
install -m 0644 \
  "$vendor_dir/browser_wasi_shim/browser_wasi_shim.mjs" \
  "$output_dir/browser_wasi_shim.mjs"
install -m 0644 \
  "$vendor_dir/browser_wasi_shim/browser_wasi_shim.mjs.map" \
  "$output_dir/browser_wasi_shim.mjs.map"
install -m 0644 "$vendor_dir/bsdtar-wasm/bsdtar.wasm" "$output_dir/bsdtar.wasm"

upstream_wasi_import='https://esm.sh/gh/haskell-wasm/browser_wasi_shim'
sed -i "s|$upstream_wasi_import|./browser_wasi_shim.mjs|g" "$output_dir/dyld.mjs"
if grep -Fq "$upstream_wasi_import" "$output_dir/dyld.mjs"; then
  die "failed to localize the browser_wasi_shim import"
fi

tar \
  --sort=name \
  --mtime="@${SOURCE_DATE_EPOCH:-0}" \
  --owner=0 \
  --group=0 \
  --numeric-owner \
  -C "$root_dir" \
  -cf - tmp \
  | zstd -q -T"${ZSTD_NBTHREADS:-0}" "-${ZSTD_CLEVEL:-19}" -o "$output_dir/rootfs.tar.zst"

archive_listing="$work_dir/rootfs.list"
tar --zstd -tf "$output_dir/rootfs.tar.zst" > "$archive_listing"
grep -Fxq 'tmp/libxeus-haskell-ghc.so' "$archive_listing"
grep -Fxq 'tmp/hslib/lib/wasm32-wasi-ghc/' "$archive_listing"
grep -Fxq "tmp/hslib/lib/$display_unit_id/XHaskell/Display.dyn_hi" "$archive_listing"
grep -Fxq "tmp/hslib/lib/wasm32-wasi-ghc/$display_library_name" "$archive_listing"
