/* global createXeusModule, importScripts */

// JupyterLite loads these resources into MEMFS before starting this worker.
// This browser test harness performs the same mounts explicitly.
importScripts('/wasm-build/xhaskell-mhs.js');

let server;

const ensureDirectory = (FS, path) => {
  try {
    FS.mkdir(path);
  } catch {}
};

const mountFiles = async (FS, manifestUrl, sourcePrefix, destinationRoot) => {
  const manifestResponse = await fetch(manifestUrl, { cache: 'no-store' });
  if (!manifestResponse.ok) {
    throw new Error(`Unable to load ${manifestUrl}: HTTP ${manifestResponse.status}`);
  }
  const paths = await manifestResponse.json();
  const files = await Promise.all(paths.map(async (relativePath) => {
    const response = await fetch(`${sourcePrefix}${relativePath}`, { cache: 'no-store' });
    if (!response.ok) {
      throw new Error(`Unable to load ${relativePath}: HTTP ${response.status}`);
    }
    return [relativePath, new Uint8Array(await response.arrayBuffer())];
  }));
  for (const [relativePath, contents] of files) {
    const segments = relativePath.split('/');
    let directory = destinationRoot;
    ensureDirectory(FS, directory);
    for (const segment of segments.slice(0, -1)) {
      directory += `/${segment}`;
      ensureDirectory(FS, directory);
    }
    FS.writeFile(`${destinationRoot}/${relativePath}`, contents);
  }
};

const initialize = async () => {
  const wasmResponse = await fetch('/wasm-build/xhaskell-mhs.wasm', {
    cache: 'no-store',
  });
  if (!wasmResponse.ok) {
    throw new Error(`Unable to load xhaskell-mhs.wasm: HTTP ${wasmResponse.status}`);
  }

  const module = await createXeusModule({
    wasmBinary: await wasmResponse.arrayBuffer(),
    locateFile: (filename) => `/wasm-build/${filename}`,
    print: (message) => console.log(message),
    printErr: (message) => console.error(message),
  });
  ensureDirectory(module.FS, '/share');
  await mountFiles(
    module.FS,
    '/test-resources/microhs.json',
    '/test-resources/microhs/',
    '/share/microhs',
  );
  for (const path of ['/usr', '/usr/lib', '/usr/lib/haskell-packages',
    '/usr/lib/haskell-packages/microhs']) {
    ensureDirectory(module.FS, path);
  }
  await mountFiles(
    module.FS,
    '/test-resources/xhaskell.json',
    '/test-resources/xhaskell/',
    '/usr/lib/haskell-packages/microhs/XHaskell',
  );
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
