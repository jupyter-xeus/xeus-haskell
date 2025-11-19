# Welcome to Xeus-Haskell

Xeus-Haskell is a Jupyter kernel for Haskell based on the native implementation of the Jupyter protocol, [xeus](https://github.com/jupyter-xeus/xeus).

## Preliminaries

To start using or developing Xeus-Haskell, please note the following:

- **MicroHs Backend:** Xeus-Haskell uses [MicroHs](https://github.com/augustss/MicroHs), a minimal implementation of Haskell. Therefore, supported libraries and extensions are limited.
- **Native Protocol:** Based on [xeus](https://github.com/jupyter-xeus/xeus) (C++), so the kernel must be compiled for the specific platform you are using.
- **WebAssembly Support:** You can compile this kernel to WebAssembly to run it directly in browsers!

## Quickstart

Since this project is not yet available on Conda-forge or Emscripten-forge, you need to build it from source. We have organized the workflows using [pixi](https://pixi.sh).

### For Native JupyterLab

~~~bash
git clone https://github.com/jupyter-xeus/xeus-haskell
cd xeus-haskell
pixi run -e default prebuild
pixi run -e default build
pixi run -e default install
pixi run -e default serve # JupyterLab is ready!
~~~

### For WebAssembly JupyterLite

~~~bash
git clone https://github.com/jupyter-xeus/xeus-haskell
cd xeus-haskell
pixi install -e prebuild
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install
# pixi run -e wasm-build fix-emscripten-links # Run this if you encounter linking errors
pixi run -e wasm-build serve # JupyterLite is ready!
~~~

## A Deep Dive into Xeus-Haskell

- [Contributing Guide](./CONTRIBUTING.md)
- [Communication between C++ and Haskell](./COMMUNICATION.md)
- [MicroHs REPL mechanism](./docs/REPL.md)
- [Build system for Xeus Kernel and MicroHs](./BUILD.md)
- [Known issues](./ISSUES.md)
