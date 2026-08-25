const { defineConfig, devices } = require('playwright/test');

module.exports = defineConfig({
  testDir: './tests',
  timeout: 180_000,
  expect: { timeout: 10_000 },
  fullyParallel: false,
  workers: 1,
  reporter: 'list',
  use: {
    baseURL: 'http://127.0.0.1:4174',
    trace: 'retain-on-failure',
  },
  webServer: {
    command: 'node harness/server.mjs',
    url: 'http://127.0.0.1:4174/healthz',
    env: {
      ...process.env,
      XHASKELL_BROWSER_TEST_PORT: '4174',
    },
    reuseExistingServer: false,
    timeout: 30_000,
  },
  projects: [{
    name: 'chromium',
    use: { ...devices['Desktop Chrome'] },
  }],
});
