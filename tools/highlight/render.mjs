import { createHighlighter } from 'shiki'
import fs from 'node:fs'
import path from 'node:path'
import { svg } from './card.mjs'

const [srcPath, outDir, title = path.basename(srcPath)] = process.argv.slice(2)
const grammar = { ...JSON.parse(fs.readFileSync(new URL('./lask.tmLanguage.json', import.meta.url), 'utf8')), name: 'lask' }
const code = fs.readFileSync(srcPath, 'utf8').replace(/\t/g, '  ').trimEnd()
const themes = ['github-dark', 'github-light']
const hl = await createHighlighter({ themes, langs: [grammar] })
fs.mkdirSync(outDir, { recursive: true })
for (const theme of themes) {
  const { tokens } = hl.codeToTokens(code, { lang: 'lask', theme })
  const out = path.join(outDir, `${path.basename(srcPath, path.extname(srcPath))}-${theme.replace('github-', '')}.svg`)
  fs.writeFileSync(out, svg(tokens, theme, title))
  console.log(out)
}
