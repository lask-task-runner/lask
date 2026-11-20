// Capture lask-tour.html, frame by frame, and encode the videos.
//
//   npm ci
//   node render.mjs stills 1,5,10      # PNG stills at those seconds, in stills/
//   node render.mjs site               # out/lask-tour.mp4 and .jpg, or into $SITE_MEDIA (1280x720, 30 fps)
//   node render.mjs readme             # ../lask-tour.gif (README cut, 960x540, 15 fps)
//
// Each frame is drawn by window.render(t) from t alone, so the result does
// not depend on how fast this machine captures. Needs Google Chrome and ffmpeg.
import puppeteer from "puppeteer-core";
import { execFileSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const CHROME = process.env.CHROME || "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const [mode = "stills", arg = "1,5,10"] = process.argv.slice(2);

async function capture(query, times, out, name) {
  fs.rmSync(out, { recursive: true, force: true });
  fs.mkdirSync(out, { recursive: true });
  const browser = await puppeteer.launch({ executablePath: CHROME, headless: "new", args: ["--force-device-scale-factor=1", "--hide-scrollbars"] });
  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 720, deviceScaleFactor: 1 });
  await page.goto("file://" + path.join(here, "lask-tour.html") + query);
  await page.evaluate(() => document.fonts.ready);
  const dur = await page.evaluate(() => window.DURATION);
  const ts = typeof times === "function" ? times(dur) : times;
  for (let i = 0; i < ts.length; i++) {
    await page.evaluate(t => window.render(t), ts[i]);
    await page.screenshot({ path: path.join(out, name(i, ts[i])) });
    if (i % 100 === 0) process.stderr.write(`${i}/${ts.length}\n`);
  }
  await browser.close();
}
const every = fps => dur => [...Array(Math.round(dur * fps)).keys()].map(i => i / fps);
const frame = i => `f_${String(i).padStart(5, "0")}.png`;

if (mode === "stills") {
  const ts = arg.split(",").map(Number);
  await capture(process.argv[4] || "", ts, path.join(here, "stills"), (i, t) => `t_${t}.png`);
} else if (mode === "site") {
  const out = path.join(here, "frames");
  await capture("", every(30), out, frame);
  const dest = process.env.SITE_MEDIA || path.join(here, "out");
  fs.mkdirSync(dest, { recursive: true });
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-framerate", "30", "-i", path.join(out, "f_%05d.png"),
    "-c:v", "libx264", "-preset", "slow", "-crf", "20", "-pix_fmt", "yuv420p", "-movflags", "+faststart",
    path.join(dest, "lask-tour.mp4")], { stdio: "inherit" });
  // the poster is the title card
  fs.copyFileSync(path.join(out, frame(Math.round(12.6 * 30))), path.join(dest, "lask-tour.png"));
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", path.join(dest, "lask-tour.png"), "-q:v", "3", path.join(dest, "lask-tour.jpg")], { stdio: "inherit" });
  fs.rmSync(path.join(dest, "lask-tour.png"));
} else if (mode === "readme") {
  const out = path.join(here, "frames");
  await capture("?short", every(15), out, frame);
  const gif = path.join(here, "..", "lask-tour.gif");
  const pal = path.join(out, "palette.png");
  const scale = "scale=960:540:flags=lanczos";
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-framerate", "15", "-i", path.join(out, "f_%05d.png"),
    "-vf", `${scale},palettegen=max_colors=96:stats_mode=diff`, pal], { stdio: "inherit" });
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-framerate", "15", "-i", path.join(out, "f_%05d.png"), "-i", pal,
    "-lavfi", `${scale}[x];[x][1:v]paletteuse=dither=bayer:bayer_scale=4:diff_mode=rectangle`, "-loop", "0", gif], { stdio: "inherit" });
}
