# Contributing to xeus-haskell

Thank you for your interest in contributing to `xeus-haskell`! This project is a subproject of Project Jupyter and follows the [Jupyter Governance](https://github.com/jupyter/governance) and [Code of Conduct](https://github.com/jupyter/governance/blob/master/conduct/code_of_conduct.md).

## Getting Started

### Prerequisites

The project uses [Pixi](https://pixi.sh/) for dependency management and workflow automation. If you haven't installed it yet:

```bash
curl -fsSL https://pixi.sh/install.sh | sh
```

### Setting Up Your Environment

1. Fork the repository on GitHub.
2. Clone your fork locally:

```bash
git clone https://github.com/<your-username>/xeus-haskell.git
cd xeus-haskell
```

3. Initialize the development environment:

```bash
pixi run -e dev prebuild
```

The `dev` environment includes all necessary tools (CMake, C++ compilers, Python for testing, etc.).

## Development Workflow

### Building the Kernel

To build and install the kernel in your local environment:

```bash
pixi run -e dev build
pixi run -e dev install
```

### Running Tests

We use `pytest` and `ctest` for verifying the kernel:

```bash
# Run C++ tests
pixi run -e dev ctest

# Run Python-based Jupyter kernel tests
pixi run -e dev pytest
```

### Working with WebAssembly (JupyterLite)

If you are contributing to the WebAssembly build, you need the `wasm-build` and `wasm-host` environments:

```bash
# Prepare the WASM host environment
pixi install -e wasm-host

# Build for WebAssembly
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install

# Run the JupyterLite server locally
pixi run -e wasm-build serve
```

## Project Structure

- `src/`: C++ source code for the kernel and REPL bridge.
- `include/`: C++ header files.
- `share/`: Haskell side of the implementation, including `Repl.hs` and MicroHs libraries.
- `test/`: Python tests for kernel functionality.
- `docs/`: Documentation files (MkDocs).

## Style Guidelines

- **C++**: Follow modern C++ practices. We use Xeus as the base library.
- **Haskell**: Since we use MicroHs, avoid dependencies or language features not supported by the MicroHs compiler.
- **Documentation**: Use Markdown for documentation and add Mermaid diagrams where helpful (as seen in `COMMUNICATION.md`).

## Submitting Changes

1. Create a new branch for your feature or bugfix.
1. Ensure all tests pass.
1. Submit a Pull Request with a clear description of the changes.

## Community

Join our public video meetings! The schedule and minutes are available on our [Team Compass](https://jupyter-xeus.github.io/).
