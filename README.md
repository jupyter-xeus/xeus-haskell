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

git clone https://github.com/jupyter-xeus/xeus-haskell
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

git clone https://github.com/jupyter-xeus/xeus-haskell
pushd xeus-haskell
pixi install -e wasm-host
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install
# pixi run -e wasm-build fix-emscripten-links # You may need this
pixi run -e wasm-build serve # JupyterLite is ready!
```

### Start Jupyter Lite with your library (Experimental feature)

To use your libraries, you mount the local directory (for example, `$PWD/example`) to `/usr/lib/haskell-packages/microhs`.
The directory structure is as follows:

```
/usr/lib/haskell-packages/microhs (local: $PWD/example)
|
+-Example
| |
| +-Hello.hs (`Example.Hello` module)
|
+-Math
| |
| +-Fibonacci.hs (`Math.Fibonacci` module)
|
.
.
.
```

The recipe is as follows:

```
pushd xeus-haskell
pixi install -e wasm-host
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build install
pixi shell -e wasm-build
pixi run -e wasm-build jupyter lite serve \
  --XeusAddon.prefix="$PWD/.envs/wasm-host" \
  --XeusAddon.mounts="$PWD/.envs/wasm-host/share/microhs:/share/microhs" \
  --XeusAddon.mounts="$PWD/example/Example:/usr/lib/haskell-packages/microhs/Example" \
  --contents notebooks/introduction_to_haskell.ipynb \
```

#### HTML Output (Experimental feature)

```haskell
import XHaskell.Display

newtype HTMLString = HTMLString String
instance Display HTMLString where
  display (HTMLString html) = DisplayData {
      mimeType = "text/html",
      content  = html
  }
instance Display String where
  display str = DisplayData "text/plain"

putStr $ show (display (HTMLString "<p style=\"color: red\">foo</p>"))
```

#### Generate Jupyter Lite as a static webpage

You can generate Jupyter Lite as a static webpage with `jupyter lite generate` command.

```
pixi run -e wasm-build jupyter lite generate \
  --XeusAddon.prefix="$PWD/.envs/wasm-host" \
  --XeusAddon.mounts="$PWD/.envs/wasm-host/share/microhs:/share/microhs" \
  # --XeusAddon.mounts="$PWD/example:/usr/lib/haskell-packages/microhs" \
  --contents notebooks/introduction_to_haskell.ipynb \
```

## Trying it online

To try out xeus-haskell interactively in your web browser, just click on the link:

[Jupyterlite for Haskell](https://jupyter-xeus.github.io/xeus-haskell)

## Documentation Manual

The project manual is generated as a PDF from the LaTeX docs and literate Haskell sources.

Generate it locally:

```bash
pixi run -e docs generate
```

This creates `xhaskell.pdf` in the repository root.

The published manual is available on GitHub Pages:

[Xeus-Haskell Manual (PDF)](https://jupyter-xeus.github.io/xeus-haskell/docs/xhaskell.pdf)

## Features

`xeus-haskell` provides a rich interactive environment for Haskell:

- **Incremental Execution**: State (definitions and types) is preserved across multiple cells.
- **Mixed Content Support**: Support for cells containing both definitions and expressions (GHCi-style).
- **Shift+Tab Introspection**: Quick access to type signatures and kinds for identifiers.
- **Tab Completion**: Intelligent completion suggestions for Haskell identifiers.
- **Rich Display System**: Integrated support for rendering HTML, LaTeX, and Markdown via the `Display` typeclass.
- **Cross-Platform**: Works natively on Linux, macOS, Windows, and in the browser via WebAssembly (JupyterLite).

## Dependencies

`xeus-haskell` depends on

- [MicroHs](https://github.com/augustss/MicroHs)
- [xeus-zmq](https://github.com/jupyter-xeus/xeus-zmq)
- [nlohmann_json](https://github.com/nlohmann/json)
- [cppzmq](https://github.com/zeromq/cppzmq)

## Contributing

See [xhaskell.tex](./xhaskell.tex) and the published [Xeus-Haskell Manual (PDF)](https://jupyter-xeus.github.io/xeus-haskell/docs/xhaskell.pdf) to know how to contribute and set up a
development environment.

## License

This software is licensed under the `Apache Software License 2.0`. See the [LICENSE](LICENSE)
file for details.
