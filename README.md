# xeus-haskell

[![Build Status](https://github.com/jupyter-xeus/xeus-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/jupyter-xeus/xeus-haskell/actions/workflows/main.yml)
[![Documentation](https://img.shields.io/badge/docs-passing-green)](https://jupyter-xeus.github.io/xeus-haskell/docs/xhaskell.pdf)

`xeus-haskell` provides Haskell Jupyter kernels built on the native
[Jupyter protocol implementation xeus](https://github.com/jupyter-xeus/xeus).

| Kernel | Display name | Native JupyterLab | JupyterLite | Backend |
| --- | --- | :---: | :---: | --- |
| `xhaskell-mhs` | Haskell (MicroHs `<version>`) | Yes | Yes | MicroHs |
| `xhaskell-ghc` | Haskell (GHC `<version>`) | No | Yes | GHC Wasm |

## Quickstart

Build and run locally through the supplied Linux amd64 Docker development container.

### Native MicroHs kernel

```sh
git clone https://github.com/jupyter-xeus/xeus-haskell
cd xeus-haskell
docker build --platform linux/amd64 -t xeus-haskell-dev .
docker run --rm -it --platform linux/amd64 \
  -v "$PWD:/workspace" -w /workspace xeus-haskell-dev
# Inside the container:
pixi run -e native native
pixi run -e native serve
```

### JupyterLite kernels

Inside the development container:

```sh
pixi run -e wasm-build wasm
pixi run -e wasm-build serve
```

The WebAssembly build installs both kernels. `xhaskell-ghc` builds a local
GHC Wasm resource bundle from the pinned native-bignum flavour described in `licenses.toml`;
it does not copy `ghc-in-browser`.

## Development

| Environment | Purpose |
| --- | --- |
| `native` | Native MicroHs build, C++/Python tests, and JupyterLab |
| `wasm-build` | Emscripten build tools and JupyterLite tasks |
| `wasm-host` | `emscripten-wasm32` target libraries |
| `browser-test` | Chromium/Playwright integration tests |
| `docs` | LaTeX manual generation |

`wasm` downloads the `wasm-host` target prefix automatically.

Run all test tiers with:

```sh
pixi run -e native test
pixi run -e wasm-build test
```

The browser tier requires the WebAssembly build and installed kernels first.
See [test/README.md](test/README.md) for its coverage matrix.

## Layout

- `cmake/`: reusable CMake modules.
- `docs/`: LaTeX manual and assets.
- `notebooks/`: example notebooks.
- `test/`: C++, Python protocol, and browser integration tests.
- `xhaskell/common/`: shared headers and browser glue.
- `xhaskell/microhs/`: MicroHs kernel implementation.
- `xhaskell/ghc/`: GHC browser kernel and resource build.

Generated native and WebAssembly files live in `build/` and `wasm-build/`.
The local GHC Wasm toolchain and generated resources are ignored beneath
`xhaskell/ghc/`.

## Runtime and licenses

Both GHC Wasm and MicroHs use non-GMP runtimes.
[`licenses.toml`](licenses.toml) centrally pins the native-bignum GHC source,
toolchain, and browser assets. Generated notices
and replacement/rebuild instructions are installed below
`share/licenses/xeus-haskell` and mounted in JupyterLite. Each distribution's
`SOURCE_OFFER.md` links to its verified corresponding-source bundle.

See [xhaskell/microhs/README.md](xhaskell/microhs/README.md) and
[xhaskell/ghc/README.md](xhaskell/ghc/README.md) for backend-specific details.

## Documentation and demo

Generate the manual with:

```sh
pixi run -e docs docs
```

The PDF is written to `docs/_build/xhaskell.pdf`. A hosted JupyterLite demo is
available at [jupyter-xeus.github.io/xeus-haskell](https://jupyter-xeus.github.io/xeus-haskell).

## License

xeus-haskell is distributed under the Apache-2.0 license. Installed kernels
include third-party notices, `licenses.toml`, and a version-specific
`SOURCE_OFFER.md` for the corresponding-source bundle.
