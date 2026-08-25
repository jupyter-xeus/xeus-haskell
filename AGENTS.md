# AI Agent Guide for xeus-haskell

Welcome, fellow agent! This document provides essential information for AI agents working on the `xeus-haskell` project.

## Project Overview

`xeus-haskell` provides Jupyter kernels for Haskell based on the native
implementation of the Jupyter protocol [xeus](https://github.com/jupyter-xeus/xeus).
It supports MicroHs natively and in WebAssembly, plus GHC in WebAssembly.

## Tech Stack

- **C++**: Core kernel implementation using `xeus` and `xeus-zmq`.
- **Haskell**: MicroHs powers the native and browser MicroHs kernel; GHC Wasm powers the browser GHC kernel.
- **Pixi**: Package management and workflow automation.
- **CMake**: Build system.
- **WebAssembly (Emscripten)**: Support for JupyterLite through `xeus-lite`.

## Environment Management

The project uses `pixi` for environment management. Key environments defined in `pixi.toml`:

- **native**: Native development environment (CMake, compilers, pytest, JupyterLab).
- **wasm-build**: Emscripten Forge toolchain and JupyterLite build tasks.
- **wasm-host**: `emscripten-wasm32` target prefix containing `xeus-lite` and
  related libraries; despite the historical environment name, it is not a
  host toolchain.
- **browser-test**: Chromium integration-test tooling.
- **docs**: Tectonic environment for building the LaTeX manual.

Each environment and its feature use the same name: `native`, `docs`,
`wasm-build`, `wasm-host`, and `browser-test`.

## Common Workflows

Agents must run `pixi run -e <environment> <task>` inside the supplied `linux/amd64` Docker development container.

### Native Development

```bash
# Download inputs, configure, build, and install the native kernel
pixi run -e native native

# Run native C++ and Python tests
pixi run -e native test

# Launch JupyterLab with the kernel
pixi run -e native serve
```

### WebAssembly / JupyterLite

```bash
# Download inputs, configure, build, and install both Wasm kernels
pixi run -e wasm-build wasm

# Run Chromium integration tests
pixi run -e wasm-build test

# Serve JupyterLite locally
pixi run -e wasm-build serve
```

### Documentation

```bash
pixi run -e docs docs
```

## Repository Structure

- `cmake/`: CMake modules and package configuration templates.
- `docs/`: LaTeX manual and documentation assets.
- `notebooks/`: Example Jupyter notebooks.
- `test/`: Native, Jupyter protocol, and WebAssembly integration tests.
- `xhaskell/common/`: Shared headers and WebAssembly browser glue.
- `xhaskell/microhs/`: MicroHs kernel implementation and kernelspec.
- `xhaskell/ghc/`: GHC browser kernel implementation and resource build.

Backend directories separate code by role: `browser/` contains JavaScript
glue, `haskell/` contains Haskell modules, `include/` and `src/` contain
C++ headers and sources, and `share/` contains installed data.

## Agent Guidelines

1. **Use Pixi**: Always prefer `pixi run` for building and testing to ensure the correct environment and dependencies are used.
2. **Cross-Compilation**: `wasm-build` runs the Emscripten Forge toolchain;
   `wasm-host` is the separately installed `emscripten-wasm32` target prefix
   consumed through `CMAKE_PREFIX_PATH`.
3. **Backends**: Keep MicroHs compatibility constraints separate from the browser-only GHC Wasm implementation.
4. **Standard Locations**: Keep build artifacts in `build/` for native and `wasm-build/` for WASM.
