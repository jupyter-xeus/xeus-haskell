# GHC browser kernel

`xhaskell-ghc` is the browser-only GHC kernel displayed as **Haskell (GHC
`<version>`)**. The WebAssembly build packages a pinned GHC Wasm runtime,
its browser WASI assets, and the project display module into a local rootfs.

All Pixi commands below run inside the `linux/amd64` Docker development container described in the top-level README.

## Build

```sh
pixi run -e wasm-build wasm
```

CMake owns the resource dependency graph. During the build it downloads the
pinned `ghc-wasm-meta` toolchain and browser assets, verifies every checksum,
and writes all generated data under `wasm-build/_deps/ghc/`. Nothing is
vendored into the repository.

## Layout

- `../../licenses.toml`: native-bignum GHC provenance, toolchain revision/checksum, and
  browser-asset URLs/checksums.
- `scripts/build-resources.sh`: the small CMake entry point for the resource pipeline.
- `scripts/prepare-inputs.sh`: downloads and verifies the pinned toolchain and browser assets.
- `scripts/build-runtime.sh`: compiles the kernel runtime and display package.
- `scripts/assemble-rootfs.sh`: creates the browser rootfs and stages its runtime assets.
- `haskell/`, `browser/`, `src/`, `share/`: kernel sources and installed data.

## Distribution and licenses

The GHC runtime uses the native bignum backend and does not include GMP. Use the Pages pipeline to make a distributable
JupyterLite site and its complete source offer:

```sh
pixi run -e wasm-build pages
```

Use `pixi run -e wasm-build source` for the verified corresponding-source
bundle alone. Generated notices, source offer, and archive are not stored in
Git; Pages and release workflows generate and publish them from pinned inputs.
