/**
 * Node.js integration test for xeus-haskell wasm kernel.
 *
 * Loads xhaskell.js / xhaskell.wasm directly, mounts the MicroHs stdlib into
 * the Emscripten virtual FS, sends Jupyter execute_request messages through the
 * xeus-lite embind API, and asserts on the replies.
 *
 * Usage (from repo root):
 *   node test/test_wasm_node.mjs
 *
 * Or via pixi (uses the wasm-build env's node):
 *   pixi run -e wasm-build node test/test_wasm_node.mjs
 */

import { readFileSync, readdirSync, statSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { pathToFileURL, fileURLToPath } from 'node:url';
import { createRequire } from 'node:module';
import { test, before } from 'node:test';
import assert from 'node:assert/strict';

const __dirname = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT  = join(__dirname, '..');

// ---------------------------------------------------------------------------
// Paths
// ---------------------------------------------------------------------------
const WASM_HOST   = join(REPO_ROOT, '.pixi/envs/wasm-host');
const KERNEL_JS   = join(WASM_HOST, 'bin/xhaskell.js');
const KERNEL_WASM = join(WASM_HOST, 'bin/xhaskell.wasm');
const MHSDIR      = join(WASM_HOST, 'share/microhs');
const MHS_LIB     = join(WASM_HOST, 'usr/lib/haskell-packages/microhs');

// ---------------------------------------------------------------------------
// Capture outgoing Jupyter messages.
// xhaskell.js is compiled with -s ENVIRONMENT=web,worker.  In worker context
// the xserver_emscripten sends replies via postMessage.  We shim the worker
// globals *before* loading the module so every call is captured.
// ---------------------------------------------------------------------------
const replies = [];
globalThis.postMessage = (msg) => { replies.push(msg); };
// Make Emscripten believe it's running inside a Worker
globalThis.WorkerGlobalScope = class {};
globalThis.self = {
  location: { href: pathToFileURL(KERNEL_JS).href },
  postMessage: globalThis.postMessage,
};

// Override fetch so that file:// and bare absolute paths can be served
// from disk. Emscripten uses fetch() to load dynamic .so libraries.
const _realFetch = globalThis.fetch;
globalThis.fetch = async (url, opts) => {
  let filePath = null;
  if (typeof url === 'string') {
    if (url.startsWith('file://')) {
      filePath = fileURLToPath(url);
    } else if (url.startsWith('/')) {
      filePath = url;
    }
  }
  if (filePath && existsSync(filePath)) {
    const data = readFileSync(filePath);
    // Return a minimal Response-like object
    const buf = data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength);
    return { ok: true, arrayBuffer: async () => buf, status: 200 };
  }
  return _realFetch(url, opts);
};

// ---------------------------------------------------------------------------
// Load the module (CommonJS export)
// ---------------------------------------------------------------------------
const require = createRequire(import.meta.url);
const createXeusModule = require(KERNEL_JS);

// ---------------------------------------------------------------------------
// Recursively copy a host directory tree into Emscripten MEMFS
// ---------------------------------------------------------------------------
function copyDirToFS(FS, hostPath, virtualPath) {
  try { FS.mkdir(virtualPath); } catch (_) {}
  for (const entry of readdirSync(hostPath)) {
    const hp = join(hostPath, entry);
    const vp = `${virtualPath}/${entry}`;
    if (statSync(hp).isDirectory()) {
      copyDirToFS(FS, hp, vp);
    } else {
      FS.writeFile(vp, readFileSync(hp));
    }
  }
}

// ---------------------------------------------------------------------------
// Build a minimal Jupyter execute_request message
// ---------------------------------------------------------------------------
let _msgCounter = 0;
function makeExecuteRequest(code) {
  return {
    header: {
      msg_id:   `test-msg-${++_msgCounter}`,
      msg_type: 'execute_request',
      session:  'test-session',
      username: 'test',
      version:  '5.3',
      date:     new Date().toISOString(),
    },
    parent_header: {},
    metadata:      {},
    content: {
      code,
      silent:           false,
      store_history:    true,
      user_expressions: {},
      allow_stdin:      false,
    },
    buffers: [],
    channel: 'shell',
  };
}

// ---------------------------------------------------------------------------
// Poll until an execute_reply for the given msg_id arrives
// ---------------------------------------------------------------------------
function waitForReply(msgId, timeoutMs = 60_000) {
  return new Promise((resolve, reject) => {
    const collected = { iopub: [], reply: null };
    const deadline  = Date.now() + timeoutMs;

    const poll = setInterval(() => {
      while (replies.length > 0) {
        const msg  = replies.shift();
        const type = msg?.header?.msg_type ?? msg?.msg_type;
        const pid  = msg?.parent_header?.msg_id;

        if (pid !== undefined && pid !== msgId) continue;

        if (type === 'execute_reply' || type === 'error') {
          collected.reply = msg;
        } else if (type) {
          collected.iopub.push(msg);
        }
      }

      if (collected.reply) { clearInterval(poll); resolve(collected); return; }
      if (Date.now() > deadline) {
        clearInterval(poll);
        reject(new Error(`Timeout waiting for reply to ${msgId}`));
      }
    }, 50);
  });
}

function extractText(collected) {
  const stdout = collected.iopub
    .filter(m => (m?.header?.msg_type ?? m?.msg_type) === 'stream' &&
                 m?.content?.name === 'stdout')
    .map(m => m?.content?.text ?? '').join('');
  const result = collected.iopub
    .filter(m => (m?.header?.msg_type ?? m?.msg_type) === 'execute_result')
    .map(m => m?.content?.data?.['text/plain'] ?? '').join('');
  return { stdout, result };
}

// ---------------------------------------------------------------------------
// Shared kernel instance (initialised once in before())
// ---------------------------------------------------------------------------
let server;

before(async () => {
  const wasmBinary = readFileSync(KERNEL_WASM);
  const Module = await createXeusModule({
    wasmBinary,
    locateFile: (f) => {
      for (const dir of [join(WASM_HOST, 'bin'), join(WASM_HOST, 'lib')]) {
        const full = join(dir, f);
        if (existsSync(full)) return pathToFileURL(full).href;
      }
      return pathToFileURL(join(WASM_HOST, 'bin', f)).href;
    },
    print:    () => {},
    printErr: () => {},
  });

  for (const seg of ['/share', '/share/microhs']) {
    try { Module.FS.mkdir(seg); } catch (_) {}
  }
  copyDirToFS(Module.FS, MHSDIR, '/share/microhs');

  for (const seg of ['/usr', '/usr/lib', '/usr/lib/haskell-packages',
                     '/usr/lib/haskell-packages/microhs']) {
    try { Module.FS.mkdir(seg); } catch (_) {}
  }
  copyDirToFS(Module.FS, MHS_LIB, '/usr/lib/haskell-packages/microhs');

  const kernel = new Module.xkernel();
  server = kernel.get_server();
  if (!server) throw new Error('get_server() returned null');
  kernel.start();

  // Brief settle time
  await new Promise(r => setTimeout(r, 500));
});

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

test('Hello, Haskell', async () => {
  // Define main
  const defReq = makeExecuteRequest('main :: IO ()\nmain = putStrLn "Hello, Haskell"');
  replies.length = 0;
  server.notify_listener(defReq);
  await new Promise(r => setTimeout(r, 3000));

  // Evaluate main
  const runReq = makeExecuteRequest('main');
  replies.length = 0;
  server.notify_listener(runReq);
  const col = await waitForReply(runReq.header.msg_id, 60_000);
  const { stdout, result } = extractText(col);
  assert.ok(
    stdout.includes('Hello, Haskell') || result.includes('Hello, Haskell'),
    `Expected "Hello, Haskell" in output, got stdout=${JSON.stringify(stdout)} result=${JSON.stringify(result)}`
  );
});

test('arithmetic (1 + 1)', async () => {
  const req = makeExecuteRequest('1 + 1');
  replies.length = 0;
  server.notify_listener(req);
  const col = await waitForReply(req.header.msg_id, 60_000);
  const { stdout, result } = extractText(col);
  const out = result || stdout;
  assert.ok(out.includes('2'), `Expected "2" in output, got ${JSON.stringify(out)}`);
});

test('function definition + application (double 21)', async () => {
  const defReq = makeExecuteRequest('double :: Int -> Int\ndouble x = x * 2');
  replies.length = 0;
  server.notify_listener(defReq);
  await new Promise(r => setTimeout(r, 2000));

  const req = makeExecuteRequest('double 21');
  replies.length = 0;
  server.notify_listener(req);
  const col = await waitForReply(req.header.msg_id, 60_000);
  const { stdout, result } = extractText(col);
  const out = result || stdout;
  assert.ok(out.includes('42'), `Expected "42" in output, got ${JSON.stringify(out)}`);
});

test(':type introspection', async () => {
  const req = makeExecuteRequest(':type length');
  replies.length = 0;
  server.notify_listener(req);
  const col = await waitForReply(req.header.msg_id, 30_000);
  const { stdout, result } = extractText(col);
  const out = result || stdout;
  assert.ok(out.length > 0, `:type produced no output`);
});
