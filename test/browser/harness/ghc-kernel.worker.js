/* global createXeusModule, importScripts */

// Exercise the production GHC Wasm module from a real browser worker.
importScripts('/wasm-build/xhaskell-ghc.js');

let server;

const locateFile = (filename) => {
  if (filename === 'xhaskell-ghc.wasm') {
    return '/wasm-build/xhaskell-ghc.wasm';
  }
  return `/wasm-build/_deps/ghc/resources/${filename}`;
};

const initialize = async () => {
  const wasmResponse = await fetch(locateFile('xhaskell-ghc.wasm'), {
    cache: 'no-store',
  });
  if (!wasmResponse.ok) {
    throw new Error(`Unable to load xhaskell-ghc.wasm: HTTP ${wasmResponse.status}`);
  }

  const module = await createXeusModule({
    wasmBinary: await wasmResponse.arrayBuffer(),
    locateFile,
    print: (message) => console.log(message),
    printErr: (message) => console.error(message),
  });
  const kernel = new module.xkernel();
  server = kernel.get_server();
  kernel.start();
  postMessage({ harness: 'ready' });
};

self.onmessage = async (event) => {
  if (event.data?.harness !== 'request') return;
  try {
    await server.notify_listener(event.data.message);
  } catch (error) {
    postMessage({
      harness: 'failure',
      requestId: event.data.message?.header?.msg_id,
      error: error?.stack || String(error),
    });
  }
};

initialize().catch((error) => {
  postMessage({ harness: 'startup-failure', error: error?.stack || String(error) });
});
