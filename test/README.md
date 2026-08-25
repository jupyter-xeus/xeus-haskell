# Test tiers

The test suite is organized by the boundary under test. Each tier applies the
same quality concepts to MicroHs and GHC at the closest portable boundary.

| Quality concept | Tier 1: C++ | Tier 2: Python | Tier 3: Chromium |
| --- | --- | --- | --- |
| Evaluation and visible output | MicroHs runtime; GHC reply adapter | MicroHs live kernel; GHC fixtures | Both live Wasm kernels |
| Mixed cells and persistence | MicroHs runtime; GHC sequential replies | MicroHs live kernel; GHC fixtures | Both live Wasm kernels |
| Errors | Both adapters | Both protocol shapes | Both live Wasm kernels |
| Completion and inspection | Both adapters | Both protocol shapes | Both live Wasm kernels |
| Completeness | Both adapters | Both protocol shapes | Both live Wasm kernels |
| HTML, LaTeX, Markdown | Both display adapters | Both protocol shapes | Both live Wasm kernels |
| Silent execution | Not applicable below Jupyter transport | Both protocol shapes | Both live Wasm kernels |

The GHC runtime is browser-only. Tier 1 tests its production C++ protocol
normalizer without Emscripten, Tier 2 validates its canonical Jupyter replies
and kernelspec, and Tier 3 executes the real GHC/GHCi Wasm runtime.

Run the tiers in order after building and installing the native kernel:

```sh
pixi run -e native test
pixi run -e wasm-build test
```

The browser tier requires the `wasm-build` output and the `wasm-host`
`emscripten-wasm32` target prefix.
It deliberately disables HTTP caching so GHC's production rootfs extraction
path is covered on every run.
The GHC browser contract blocks non-local requests, ensuring its WASI
implementation and root-filesystem extractor are fully self-hosted. MicroHs is tested using its non-GMP runtime. GHC uses the native bignum backend without GMP.

Run `pixi run -e wasm-build pages` to build and audit the distributable
JupyterLite site, including the Xeus license mount and corresponding-source bundle.
