import {
  createReadStream, existsSync, readFileSync, readdirSync, statSync,
} from 'node:fs';
import { createServer } from 'node:http';
import { extname, join, normalize, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';

const repoRoot = resolve(fileURLToPath(new URL('../../..', import.meta.url)));
const port = Number.parseInt(process.env.XHASKELL_BROWSER_TEST_PORT || '4173', 10);
const microHsRoot = join(repoRoot, '.pixi/envs/wasm-host/share/microhs');
const xhaskellRoot = join(repoRoot, 'xhaskell/microhs/haskell/XHaskell');
const ghcKernelTemplate = JSON.parse(readFileSync(join(
  repoRoot,
  'xhaskell/ghc/share/jupyter/kernels/xhaskell-ghc/kernel.json.in',
)));
const requiredFiles = [
  'wasm-build/xhaskell-ghc.js',
  'wasm-build/xhaskell-ghc.wasm',
  'wasm-build/xhaskell-mhs.js',
  'wasm-build/xhaskell-mhs.wasm',
  ...Object.keys(ghcKernelTemplate.metadata.shared).map(
    (filename) => `wasm-build/_deps/ghc/resources/${filename}`,
  ),
  '.pixi/envs/wasm-host/share/microhs',
];

for (const relativePath of requiredFiles) {
  if (!existsSync(join(repoRoot, relativePath))) {
    throw new Error(
      "Missing " + relativePath + "; run `pixi run -e wasm-build wasm` first.",
    );
  }
}

const contentTypes = new Map([
  ['.html', 'text/html; charset=utf-8'],
  ['.js', 'text/javascript; charset=utf-8'],
  ['.mjs', 'text/javascript; charset=utf-8'],
  ['.wasm', 'application/wasm'],
  ['.zst', 'application/zstd'],
]);

const listFiles = (root, relative = '') => readdirSync(join(root, relative))
  .flatMap((entry) => {
    const path = join(relative, entry);
    return statSync(join(root, path)).isDirectory() ? listFiles(root, path) : [path];
  })
  .map((path) => path.replaceAll('\\', '/'));

const resourceMounts = [
  {
    manifest: '/test-resources/microhs.json',
    prefix: '/test-resources/microhs/',
    root: microHsRoot,
  },
  {
    manifest: '/test-resources/xhaskell.json',
    prefix: '/test-resources/xhaskell/',
    root: xhaskellRoot,
  },
];

const server = createServer((request, response) => {
  const requestUrl = new URL(request.url || '/', `http://${request.headers.host}`);
  if (requestUrl.pathname === '/healthz') {
    response.writeHead(200, { 'Content-Type': 'text/plain; charset=utf-8' });
    response.end('ok');
    return;
  }
  if (requestUrl.pathname === '/favicon.ico') {
    response.writeHead(204, { 'Cache-Control': 'no-store' });
    response.end();
    return;
  }

  const manifestMount = resourceMounts.find(
    (mount) => requestUrl.pathname === mount.manifest,
  );
  if (manifestMount) {
    response.writeHead(200, {
      'Content-Type': 'application/json; charset=utf-8',
      'Cache-Control': 'no-store',
    });
    response.end(JSON.stringify(listFiles(manifestMount.root)));
    return;
  }

  const resourceMount = resourceMounts.find(
    (mount) => requestUrl.pathname.startsWith(mount.prefix),
  );

  const pathname = requestUrl.pathname === '/'
    ? '/test/browser/harness/kernel-harness.html'
    : decodeURIComponent(requestUrl.pathname);
  const relativePath = normalize(
    resourceMount ? pathname.slice(resourceMount.prefix.length) : pathname,
  ).replace(/^[/\\]+/, '');
  const fileRoot = resourceMount?.root || repoRoot;
  const filePath = resolve(fileRoot, relativePath);
  if (filePath !== fileRoot && !filePath.startsWith(`${fileRoot}${sep}`)) {
    response.writeHead(403);
    response.end('Forbidden');
    return;
  }
  if (!existsSync(filePath)) {
    response.writeHead(404);
    response.end('Not found');
    return;
  }

  response.writeHead(200, {
    'Content-Type': contentTypes.get(extname(filePath)) || 'application/octet-stream',
    // Force each run through the production rootfs fetch and extraction path.
    'Cache-Control': 'no-store, no-cache, must-revalidate',
    Pragma: 'no-cache',
    Expires: '0',
  });
  createReadStream(filePath).pipe(response);
});

server.listen(port, '127.0.0.1', () => {
  process.stdout.write(`xhaskell browser test server: http://127.0.0.1:${port}\n`);
});

const stop = () => server.close(() => process.exit(0));
process.on('SIGINT', stop);
process.on('SIGTERM', stop);
