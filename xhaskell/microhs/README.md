# MicroHs kernel

`xhaskell-mhs` is the MicroHs-backed kernel. It is available in native
JupyterLab and in JupyterLite, where it is displayed as
**Haskell (MicroHs `<version>`)**.

## Build

For native JupyterLab:

```sh
pixi run -e native native
pixi run -e native serve
```

For JupyterLite:

```sh
pixi run -e wasm-build wasm
pixi run -e wasm-build serve
```

`cmake/MicroHs.cmake` downloads the pinned MicroHs release and builds the REPL
runtime. Build products remain in the top-level `build/` and `wasm-build/`
directories.

## Runtime

The kernel holds a persistent `ReplCtx`, so successful declarations remain
available to later cells. It supports expressions, IO, declaration prefixes
followed by executable code, completion, inspection, completeness checks, and
rich display output. Startup evaluates a small internal expression to warm the
compiler before accepting user cells.

`XHaskell.Display` emits HTML, LaTeX, and Markdown as Jupyter `display_data`.
Plain values remain `execute_result`, and normal output remains stream output.
Language compatibility follows MicroHs rather than full GHC/GHCi.

## Layout

- `browser/`: Emscripten environment setup and module paths.
- `haskell/`: REPL implementation and display module.
- `include/`, `src/`: C++ interfaces, interpreter, and entry points.
- `share/`: kernelspec and installed data.
- `licenses/`: generated runtime notices; distribution provenance is in `../../licenses.toml`.

JupyterLite mounts the MicroHs runtime at `/share/microhs` and the project
display module under `/usr/lib/haskell-packages/microhs`.

## Licenses and tests

Both variants are built without GMP. MicroHs uses its non-GMP runtime, so no
GMP header, library, or GMP corresponding-source archive is required for this
backend. The installed `SOURCE_OFFER.md` still identifies the complete
distribution-level source bundle, including the separate GHC runtime inputs.

Coverage is shared across `test/cpp/microhs/`, `test/python/`, and
`test/browser/`.
