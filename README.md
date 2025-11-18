# ![xeus-haskell](xeus-logo.svg)

[![Build Status](https://github.com/jupyter-xeus/xeus-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/jupyter-xeus/xeus-haskell/actions/workflows/main.yml)

`xeus-haskell` is a Jupyter kernel for haskell based on the native implementation of the
Jupyter protocol [xeus](https://github.com/jupyter-xeus/xeus).

<img width="800" height="500" alt="image" src="https://github.com/user-attachments/assets/c9efa18c-13bb-4c6e-a133-dac0281a66cf" />


## Quickstart

### For native JupyterLab

```
# Install Pixi
# curl -fsSL https://pixi.sh/install.sh | sh

git clone https://github.com/tani/xeus-haskell
pushd xeus-haskell
pixi run -e default prebuild
pixi run -e default build
pixi run -e default install
pixi run -e default serve # JupyterLab is ready!
```

### For webassembly JupyterLite

```
# Install Pixi
# curl -fsSL https://pixi.sh/install.sh | sh

git clone https://github.com/tani/xeus-haskell
pushd xeus-haskell
pixi install -e wasm-host
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install
# pixi run -e wasm-build fix-emscripten-links # You may need this
pixi run -e wasm-build serve # JupyterLite is ready!
```

## Trying it online

To try out xeus-haskell interactively in your web browser, just click on the link:

[Jupyterlite for Haskell](https://jupyter-xeus.github.io/xeus-haskell)

## Dependencies

`xeus-haskell` depends on

- [MicroHs](https://github.com/augustss/MicroHs)
- [xeus-zmq](https://github.com/jupyter-xeus/xeus-zmq)
- [xtl](https://github.com/xtensor-stack/xtl)
- [nlohmann_json](https://github.com/nlohmann/json)
- [cppzmq](https://github.com/zeromq/cppzmq)

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) to know how to contribute and set up a
development environment.

## License

This software is licensed under the `Apache Software License 2.0`. See the [LICENSE](LICENSE)
file for details.
