// node render.js <page.html> <fps> [comma-separated times, or - for every frame] [query]
const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
(async () => {
  const [page = 'pv.html', fpsArg = '30', onlyArg, query = ''] = process.argv.slice(2);
  const fps = +fpsArg, only = onlyArg && onlyArg !== '-' ? onlyArg.split(',').map(Number) : null;
  const out = path.join(__dirname, 'frames'); fs.mkdirSync(out, { recursive: true });
  const b = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new',
    args: ['--force-device-scale-factor=1', '--hide-scrollbars'] });
  const p = await b.newPage();
  await p.goto('file://' + path.join(__dirname, page) + query);
  const [w, h] = await p.evaluate(() => window.VIEW || [1280, 720]);
  await p.setViewport({ width: w, height: h, deviceScaleFactor: 1 });
  const dur = await p.evaluate(() => window.DURATION);
  const times = only || [...Array(Math.round(dur * fps)).keys()].map(i => i / fps);
  for (let i = 0; i < times.length; i++) {
    await p.evaluate(t => window.render(t), times[i]);
    const name = only ? `snap_${times[i]}.png` : `f_${String(i).padStart(4, '0')}.png`;
    await p.screenshot({ path: path.join(only ? __dirname : out, name) });
  }
  await b.close();
})();
