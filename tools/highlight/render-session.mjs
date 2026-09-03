// Renders a captured terminal transcript to the same SVG card as render.mjs.
// Input is plain text: lines beginning with "$ " are what the user typed, the
// rest is whatever lask printed.
import fs from 'node:fs'
import path from 'node:path'
import { svg } from './card.mjs'

const PALETTE = {
  'github-dark': { fg: '#e6edf3', dim: '#8b949e', green: '#7ee787', red: '#ff7b72', purple: '#d2a8ff', blue: '#79c0ff', orange: '#ffa657' },
  'github-light': { fg: '#1f2328', dim: '#59636e', green: '#116329', red: '#cf222e', purple: '#8250df', blue: '#0550ae', orange: '#953800' },
}

const TIMESTAMP = /^(\d{4}-\d{2}-\d{2}T[\d:.]+Z )(\[[^\]]*\] )?/
const ENV_TAG = /^\[(#[^\]]*)\](\s*)$/

// A line the user typed: "$ lask eval --stdout-encode text cowsay-hello Lask".
function promptLine(rest, p) {
  const out = [{ content: '$ ', color: p.green }]
  for (const word of rest.split(/(\s+)/)) {
    if (/^\s+$/.test(word) || word === '') out.push({ content: word, color: p.fg })
    else if (word === 'lask') out.push({ content: word, color: p.purple })
    else if (word.startsWith('--')) out.push({ content: word, color: p.orange })
    else out.push({ content: word, color: p.fg })
  }
  return out
}

// Everything lask emits after the "<timestamp> [#env]" prefix.
function diagnosticBody(body, p) {
  if (body.startsWith('$ ')) return [{ content: '$ ', color: p.red }, { content: body.slice(2), color: p.fg }]
  if (body.startsWith('1| ')) return [{ content: '1| ', color: p.dim }, { content: body.slice(3), color: p.fg }]
  if (body.startsWith('2| ')) return [{ content: '2| ', color: p.dim }, { content: body.slice(3), color: p.red }]
  if (/^exit 0\s*$/.test(body)) return [{ content: body, color: p.green }]
  if (/^exit \d/.test(body)) return [{ content: body, color: p.red }]
  return [{ content: body, color: p.fg }]
}

function tokenize(text, theme) {
  const p = PALETTE[theme]
  return text.split('\n').map(line => {
    if (line === '') return []
    if (line.startsWith('$ ')) return promptLine(line.slice(2), p)

    const m = TIMESTAMP.exec(line)
    if (!m) return [{ content: line, color: p.fg }]

    const tokens = [{ content: m[1], color: p.dim }]
    if (m[2]) {
      const env = ENV_TAG.exec(m[2])
      tokens.push({ content: '[', color: p.dim }, { content: env[1], color: p.green },
                   { content: ']', color: p.dim }, { content: env[2], color: p.dim })
    }
    return tokens.concat(diagnosticBody(line.slice(m[0].length), p))
  })
}

const [srcPath, outDir, title = path.basename(srcPath)] = process.argv.slice(2)
const text = fs.readFileSync(srcPath, 'utf8').replace(/\t/g, '  ').replace(/\r/g, '').trimEnd()
fs.mkdirSync(outDir, { recursive: true })
for (const theme of ['github-dark', 'github-light']) {
  const out = path.join(outDir, `${path.basename(srcPath, path.extname(srcPath))}-${theme.replace('github-', '')}.svg`)
  fs.writeFileSync(out, svg(tokenize(text, theme), theme, title))
  console.log(out)
}
