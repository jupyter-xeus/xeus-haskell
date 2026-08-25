const { expect, test } = require('playwright/test');

const messagesOf = (response, messageType, streamName) => response.iopub.filter(
  (message) => message?.header?.msg_type === messageType
    && (!streamName || message?.content?.name === streamName),
);

const textOutput = (response) => [
  ...messagesOf(response, 'execute_result').map(
    (message) => message?.content?.data?.['text/plain'] || '',
  ),
  ...messagesOf(response, 'stream', 'stdout').map(
    (message) => message?.content?.text || '',
  ),
].join('\n');

const displayOf = (response, mimeType) => response.iopub.find(
  (message) => message?.header?.msg_type === 'display_data'
    && Object.hasOwn(message?.content?.data || {}, mimeType),
);

const request = (page, method, ...args) => page.evaluate(
  ({ methodName, methodArgs }) => window.kernelHarness[methodName](...methodArgs),
  { methodName: method, methodArgs: args },
);

const openKernel = async (page, kernel) => {
  const pageErrors = [];
  page.on('pageerror', (error) => pageErrors.push(error.message));
  await page.goto(`/test/browser/harness/kernel-harness.html?kernel=${kernel}`);
  await page.evaluate(() => window.kernelHarness.ready);
  expect(pageErrors).toEqual([]);
};

const exerciseSharedContract = async (page, kernel) => {
  const number = await request(page, 'execute', '1 + 1');
  expect(number.reply.content.status).toBe('ok');
  expect(textOutput(number)).toMatch(/\b2\b/);

  const declaration = await request(page, 'execute', 'sharedAnswer = 40 + 2');
  expect(declaration.reply.content.status).toBe('ok');
  const value = await request(page, 'execute', 'sharedAnswer');
  expect(value.reply.content.status).toBe('ok');
  expect(textOutput(value)).toMatch(/\b42\b/);

  const mixed = await request(page, 'execute', [
    'sharedDouble value =',
    '  value * 2',
    'sharedDouble 21',
  ].join('\n'));
  expect(mixed.reply.content.status, JSON.stringify(mixed.iopub)).toBe('ok');
  expect(textOutput(mixed)).toMatch(/\b42\b/);

  const io = await request(page, 'execute', `putStrLn "Hello from ${kernel}"`);
  expect(io.reply.content.status).toBe('ok');
  expect(textOutput(io)).toContain(`Hello from ${kernel}`);

  const multipleIo = await request(page, 'execute', [
    'sharedGreeting = "shared-first"',
    'putStrLn sharedGreeting',
    'putStrLn "shared-second"',
  ].join('\n'));
  expect(multipleIo.reply.content.status, JSON.stringify(multipleIo.iopub)).toBe('ok');
  expect(textOutput(multipleIo)).toContain('shared-first');
  expect(textOutput(multipleIo)).toContain('shared-second');

  const completion = await request(page, 'complete', 'sharedAns', 9);
  expect(completion.reply.content.status).toBe('ok');
  expect(completion.reply.content.matches).toContain('sharedAnswer');

  const inspection = await request(page, 'inspect', 'sharedAnswer', 6);
  expect(inspection.reply.content.status).toBe('ok');
  expect(inspection.reply.content.found).toBe(true);
  expect(inspection.reply.content.data['text/plain'].length).toBeGreaterThan(0);

  const incomplete = await request(page, 'isComplete', '[1, 2, 3');
  expect(incomplete.reply.content.status).toBe('incomplete');

  const richDisplays = [
    ['text/html', '<strong>HTML</strong>'],
    ['text/latex', '$x^2$'],
    ['text/markdown', '**Markdown**'],
  ];
  for (const [mimeType, content] of richDisplays) {
    const framed = `putStrLn "\\x02${mimeType}\\x1F${content}\\x03"`;
    const response = await request(page, 'execute', framed);
    expect(response.reply.content.status).toBe('ok');
    expect(displayOf(response, mimeType)?.content?.data?.[mimeType]).toBe(content);
    expect(messagesOf(response, 'execute_result')).toHaveLength(0);
  }

  const silent = await request(
    page,
    'execute',
    'putStrLn "\\x02text/html\\x1F<b>silent</b>\\x03"',
    { silent: true },
  );
  expect(silent.reply.content.status).toBe('ok');
  expect(messagesOf(silent, 'display_data')).toHaveLength(0);
  expect(messagesOf(silent, 'execute_result')).toHaveLength(0);
  expect(messagesOf(silent, 'stream')).toHaveLength(0);
};

test('GHC satisfies the shared browser contract and extensions', async ({ page }) => {
  const externalRequests = [];
  await page.context().route('**/*', async (route) => {
    const url = new URL(route.request().url());
    if (url.hostname === '127.0.0.1') {
      await route.continue();
      return;
    }
    externalRequests.push(url.href);
    await route.abort('blockedbyclient');
  });

  await openKernel(page, 'ghc');
  await exerciseSharedContract(page, 'ghc');

  const largeInteger = await request(
    page,
    'execute',
    '2 ^ (128 :: Int) :: Integer',
  );
  expect(largeInteger.reply.content.status, JSON.stringify(largeInteger.iopub)).toBe('ok');
  expect(textOutput(largeInteger)).toContain('340282366920938463463374607431768211456');

  const multipleDeclarations = await request(page, 'execute', [
    'mixedFirst = 20',
    'mixedSecond = 22',
    'mixedFirst + mixedSecond',
  ].join('\n'));
  expect(
    multipleDeclarations.reply.content.status,
    JSON.stringify(multipleDeclarations.iopub),
  ).toBe('ok');
  expect(textOutput(multipleDeclarations)).toMatch(/\b42\b/);

  const failedTail = await request(page, 'execute', [
    'keptAfterFailure = 73',
    'missingMixedName',
  ].join('\n'));
  expect(failedTail.reply.content.status).toBe('error');
  const kept = await request(page, 'execute', 'keptAfterFailure');
  expect(textOutput(kept)).toMatch(/\b73\b/);

  const completeness = await request(page, 'isComplete', [
    'completeMixed = 20',
    'completeMixed + 22',
  ].join('\n'));
  expect(completeness.reply.content.status).toBe('complete');

  const imported = await request(page, 'execute', 'import XHaskell.Display');
  expect(imported.reply.content.status).toBe('ok');
  const declared = await request(page, 'execute', [
    'newtype HtmlValue = HtmlValue String',
    'instance Display HtmlValue where',
    '  display (HtmlValue value) = DisplayData "text/html" value',
  ].join('\n'));
  expect(declared.reply.content.status, JSON.stringify(declared.iopub)).toBe('ok');

  const displayed = await request(
    page,
    'execute',
    'display (HtmlValue "<em>Display API</em>")',
  );
  expect(displayOf(displayed, 'text/html')?.content?.data?.['text/html'])
    .toBe('<em>Display API</em>');

  const multipleFrames = await request(
    page,
    'execute',
    'putStrLn "before\\x02text/html\\x1F<b>one</b>\\x03middle'
      + '\\x02text/markdown\\x1F**two**\\x03\\&after"',
  );
  expect(textOutput(multipleFrames)).toContain('before');
  expect(textOutput(multipleFrames)).toContain('middle');
  expect(textOutput(multipleFrames)).toContain('after');
  expect(displayOf(multipleFrames, 'text/html')?.content?.data?.['text/html'])
    .toBe('<b>one</b>');
  expect(displayOf(multipleFrames, 'text/markdown')?.content?.data?.['text/markdown'])
    .toBe('**two**');

  const malformed = await request(
    page,
    'execute',
    'putStrLn "\\x02text/html\\x1Funterminated"',
  );
  expect(textOutput(malformed)).toContain('unterminated');
  expect(externalRequests).toEqual([]);
});

test('MicroHs satisfies the shared browser-kernel contract', async ({ page }) => {
  await openKernel(page, 'microhs');
  await exerciseSharedContract(page, 'microhs');

  const declaration = await request(
    page,
    'execute',
    'foreign import capi "want_gmp" xhWantGmp :: Int',
  );
  expect(declaration.reply.content.status, JSON.stringify(declaration.iopub)).toBe('ok');
  const gmpEnabled = await request(page, 'execute', 'xhWantGmp');
  expect(gmpEnabled.reply.content.status, JSON.stringify(gmpEnabled.iopub)).toBe('ok');
  expect(textOutput(gmpEnabled)).toContain('0');
});
