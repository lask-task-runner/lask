const puppeteer = require('puppeteer-core');
const path = require('path'), fs = require('fs');
(async () => {
  // node render.js [fps] [comma-separated times, or - for every frame] [query, e.g. ?short]
  const fps = +(process.argv[2] || 30), only = process.argv[3] && process.argv[3] !== '-' ? process.argv[3] : null, query = process.argv[4] || '';
  const out = path.join(__dirname, 'frames'); fs.mkdirSync(out, { recursive: true });
  const b = await puppeteer.launch({ executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: 'new',
    args: ['--force-device-scale-factor=1', '--hide-scrollbars'] });
  const p = await b.newPage(); await p.setViewport({ width: 1280, height: 720, deviceScaleFactor: 1 });
  await p.goto('file://' + path.join(__dirname, 'pv.html') + query);
  const dur = await p.evaluate(() => window.DURATION);
  const times = only ? only.split(',').map(Number) : [...Array(Math.round(dur * fps)).keys()].map(i => i / fps);
  for (let i = 0; i < times.length; i++) {
    await p.evaluate(t => window.render(t), times[i]);
    const name = only ? `snap_${times[i]}.png` : `f_${String(i).padStart(4, '0')}.png`;
    await p.screenshot({ path: path.join(only ? __dirname : out, name) });
  }
  await b.close();
})();
