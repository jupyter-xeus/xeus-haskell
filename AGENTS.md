# AI Agent Guide for xeus-haskell

Welcome, fellow agent! This document provides essential information for AI agents working on the `xeus-haskell` project.

## Project Overview

`xeus-haskell` is a Jupyter kernel for Haskell based on the native implementation of the Jupyter protocol [xeus](https://github.com/jupyter-xeus/xeus). It uses [MicroHs](https://github.com/augustss/MicroHs) as the Haskell implementation.

## Tech Stack

- **C++**: Core kernel implementation using `xeus` and `xeus-zmq`.
- **Haskell (MicroHs)**: Used for Haskell code execution.
- **Pixi**: Package management and workflow automation.
- **CMake**: Build system.
- **WebAssembly (Emscripten)**: Support for JupyterLite through `xeus-lite`.

## Environment Management

The project uses `pixi` for environment management. Key environments defined in `pixi.toml`:

- **default**: Native development environment (CMake, compilers, pytest, JupyterLab).
- **wasm-build**: Tools for cross-compiling to WebAssembly (Emscripten).
- **wasm-host**: Emscripten-specific dependencies (xeus-lite).
- **docs**: MkDocs environment for documentation.

## Common Workflows

Agents should use `pixi run -e <environment> <task>` to perform operations.

### Native Development

```bash
# Prepare build directory
pixi run prebuild

# Build the kernel
pixi run build

# Install the kernel
pixi run install

# Run tests
pixi run ctest
pixi run pytest

# Launch JupyterLab with the kernel
pixi run serve
```

### WebAssembly / JupyterLite

```bash
# Prepare host dependencies
pixi install -e wasm-host

# Build for WebAssembly
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install

# Serve JupyterLite locally
pixi run -e wasm-build serve
```

## Repository Structure

- `src/`: C++ source files for the kernel.
- `include/`: C++ header files.
- `share/`: Data files, including Haskell/MicroHs libraries.
- `test/`: Python-based kernel tests.
- `notebooks/`: Example Jupyter notebooks.
- `cmake/`: CMake modules.

## Agent Guidelines

1. **Use Pixi**: Always prefer `pixi run` for building and testing to ensure the correct environment and dependencies are used.
2. **Cross-Compilation**: When working on WebAssembly features, remember that binaries for `wasm-host` are built in the `wasm-build` environment.
3. **MicroHs**: Be aware that this kernel specifically uses MicroHs, which may have differences from GHC.
4. **Standard Locations**: Keep build artifacts in `build/` for native and `wasm-build/` for WASM.
