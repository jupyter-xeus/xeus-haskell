let xhaskellGhcRuntimePromise = null;
let xhaskellGhcCurrentOutput = null;
let xhaskellGhcRequestChain = Promise.resolve();

const xhaskellGhcResultMarker = '__XHASKELL_GHC_RESULT_83f31cbe__';

const xhaskellGhcAppendLine = function(stream, message) {
  if (xhaskellGhcCurrentOutput) {
    xhaskellGhcCurrentOutput[stream].push(String(message));
  }
};

const xhaskellGhcFetchBytes = async function(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Unable to load ${url}: HTTP ${response.status}`);
  }
  return new Uint8Array(await response.arrayBuffer());
};

const xhaskellGhcCreateDefaultRuntime = async function() {
  const assetUrl = (filename) => Module['locateFile'](filename);
  const loadModules = Module['xhaskellGhcLoadModules'] || (async () => {
    const [wasi, dyld] = await Promise.all([
      import(assetUrl('browser_wasi_shim.mjs')),
      import(assetUrl('dyld.mjs')),
    ]);
    return { wasi, dyld };
  });
  const loadBytes = Module['xhaskellGhcLoadBytes'] || xhaskellGhcFetchBytes;
  const [{ wasi, dyld }, bsdtarBytes, rootfsBytes] = await Promise.all([
    loadModules(),
    loadBytes(assetUrl('bsdtar.wasm')),
    loadBytes(assetUrl('rootfs.tar.zst')),
  ]);

  const rootfs = new wasi.PreopenDirectory('/', []);
  const bsdtarWasi = new wasi.WASI(
    ['bsdtar.wasm', '-x'],
    [],
    [
      new wasi.OpenFile(new wasi.File(new Uint8Array(), { readonly: true })),
      wasi.ConsoleStdout.lineBuffered((message) => console.info(message)),
      wasi.ConsoleStdout.lineBuffered((message) => console.warn(message)),
      rootfs,
    ],
    { debug: false },
  );
  const instantiated = await WebAssembly.instantiate(bsdtarBytes, {
    wasi_snapshot_preview1: bsdtarWasi.wasiImport,
  });
  bsdtarWasi.fds[0] = new wasi.OpenFile(
    new wasi.File(rootfsBytes, { readonly: true }),
  );
  bsdtarWasi.start(instantiated.instance);

  const linker = await dyld.main({
    rpc: new dyld.DyLDBrowserHost({
      rootfs,
      stdout: (message) => xhaskellGhcAppendLine('stdout', message),
      stderr: (message) => xhaskellGhcAppendLine('stderr', message),
    }),
    searchDirs: ['/tmp/clib', '/tmp/hslib/lib/wasm32-wasi-ghc'],
    mainSoPath: '/tmp/libxeus-haskell-ghc.so',
    args: ['libxeus-haskell-ghc.so', '+RTS', '-c', '-RTS'],
    isIserv: false,
  });
  return linker.exportFuncs.xhaskellGhcMain(
    '/tmp/hslib/lib',
    Module['xhaskellGhcArgs'] || '-v0 -XExtendedDefaultRules -XNoMonomorphismRestriction',
  );
};

const xhaskellGhcFailure = function(error, output) {
  const name = error && error.name ? String(error.name) : 'Error';
  const message = error && error.message ? String(error.message) : String(error);
  const compilerMessage = output.stderr.join('\n');
  const stack = error && error.stack ? String(error.stack) : `${name}: ${message}`;
  return {
    ok: false,
    ename: /ExitFailure/.test(message) ? 'GHCError' : name,
    evalue: compilerMessage || message,
    traceback: compilerMessage ? [...output.stderr] : stack.split('\n'),
    stdout: output.stdout.length === 0 ? '' : `${output.stdout.join('\n')}\n`,
    stderr: '',
  };
};

const xhaskellGhcRequestOnce = async function(dispatch, operation, code, cursor, detail) {
  const output = { stdout: [], stderr: [] };
  try {
    xhaskellGhcCurrentOutput = output;
    const envelope = JSON.parse(await dispatch(operation, code, cursor, detail));
    const payload = envelope.payload || {};
    if (operation !== 'execute') {
      return { ok: true, ...payload };
    }

    const markerIndex = output.stdout.indexOf(xhaskellGhcResultMarker);
    const stdoutLines = markerIndex < 0 ? output.stdout : output.stdout.slice(0, markerIndex);
    const resultLines = markerIndex < 0 ? [] : output.stdout.slice(markerIndex + 1);
    const commandStdout = payload.stdout || '';
    return {
      ok: true,
      has_result: markerIndex >= 0,
      result: resultLines.join('\n'),
      stdout: `${stdoutLines.length ? `${stdoutLines.join('\n')}\n` : ''}${commandStdout}`,
      stderr: output.stderr.length === 0 ? '' : `${output.stderr.join('\n')}\n`,
    };
  } catch (error) {
    return xhaskellGhcFailure(error, output);
  } finally {
    xhaskellGhcCurrentOutput = null;
  }
};

const xhaskellGhcInitialize = function() {
  if (!xhaskellGhcRuntimePromise) {
    const createRuntime = Module['xhaskellGhcCreateRuntime'] || xhaskellGhcCreateDefaultRuntime;
    xhaskellGhcRuntimePromise = (async () => {
      const dispatch = await createRuntime({
        stdout: (message) => xhaskellGhcAppendLine('stdout', message),
        stderr: (message) => xhaskellGhcAppendLine('stderr', message),
      });
      const warmup = await xhaskellGhcRequestOnce(dispatch, 'warmup', '1 + 1', 0, 0);
      if (!warmup.ok) {
        const error = new Error(`GHC warm-up failed: ${warmup.evalue}`);
        error.name = warmup.ename;
        throw error;
      }
      return dispatch;
    })();
  }
  return xhaskellGhcRuntimePromise;
};

Module['xhaskellGhcRequest'] = function(operation, code, cursor = 0, detail = 0) {
  const request = async () => {
    try {
      const dispatch = await xhaskellGhcInitialize();
      return JSON.stringify(
        await xhaskellGhcRequestOnce(dispatch, operation, code, cursor, detail),
      );
    } catch (error) {
      return JSON.stringify(xhaskellGhcFailure(error, { stdout: [], stderr: [] }));
    }
  };
  const result = xhaskellGhcRequestChain.then(request, request);
  xhaskellGhcRequestChain = result.then(() => undefined, () => undefined);
  return result;
};

Module['postRun'] = Module['postRun'] || [];
Module['postRun'].push(function() {
  const originalGetServer = Module['xkernel'].prototype.get_server;
  Module['xkernel'].prototype.get_server = function() {
    const server = originalGetServer.call(this);
    if (!server['xhaskellGhcAsyncNotifyInstalled']) {
      server.notify_listener = (message) =>
        Module['xhaskellGhcNotifyListener'](server, message);
      server['xhaskellGhcAsyncNotifyInstalled'] = true;
    }
    return server;
  };
});

Module['preRun'] = Module['preRun'] || [];
Module['preRun'].push(function() {
  const dependency = 'xhaskell-ghc-warmup';
  addRunDependency(dependency);
  xhaskellGhcInitialize().then(
    () => removeRunDependency(dependency),
    (error) => {
      console.error(error);
      removeRunDependency(dependency);
    },
  );
});
