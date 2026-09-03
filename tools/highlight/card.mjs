const FONT = 'ui-monospace,SFMono-Regular,&#34;SF Mono&#34;,Menlo,Consolas,&#34;Liberation Mono&#34;,monospace'
const FS = 13.5, LH = 21, CW = FS * 0.6, PAD_X = 22, PAD_Y = 18, BAR = 36, RADIUS = 10

export const CHROME = {
  'github-dark':  { bg: '#0d1117', border: '#30363d', bar: '#161b22', title: '#8b949e', dots: ['#ff5f57', '#febc2e', '#28c840'] },
  'github-light': { bg: '#ffffff', border: '#d0d7de', bar: '#f6f8fa', title: '#59636e', dots: ['#ff5f57', '#febc2e', '#28c840'] },
}

const esc = s => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')

export function svg(tokens, theme, title) {
  const c = CHROME[theme]
  const cols = Math.max(...tokens.map(l => l.reduce((n, t) => n + t.content.length, 0)), title.length + 12)
  const w = Math.ceil(cols * CW + PAD_X * 2)
  const h = Math.ceil(BAR + PAD_Y * 2 + tokens.length * LH)
  // Every glyph gets its own x, so the grid holds no matter which monospace
  // font the reader's machine substitutes. Trailing blanks are dropped: they
  // would only pad the x list.
  const lines = tokens.map((line, i) => {
    const y = (BAR + PAD_Y + LH * i + FS).toFixed(1)
    const text = line.map(t => t.content).join('').replace(/\s+$/, '')
    if (text === '') return ''
    let n = 0, spans = ''
    for (const t of line) {
      const body = t.content.slice(0, Math.max(0, text.length - n))
      n += t.content.length
      if (body === '') continue
      const bold = t.fontStyle & 2 ? ' font-weight="600"' : ''
      const ital = t.fontStyle & 1 ? ' font-style="italic"' : ''
      spans += `<tspan fill="${t.color || c.title}"${bold}${ital}>${esc(body)}</tspan>`
    }
    const xs = Array.from(text, (_, k) => (PAD_X + k * CW).toFixed(1)).join(' ')
    return `    <text xml:space="preserve" y="${y}" x="${xs}">${spans}</text>`
  }).filter(Boolean).join('\n')

  return `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}" font-family="${FONT}" font-size="${FS}">
  <rect x="0.5" y="0.5" width="${w - 1}" height="${h - 1}" rx="${RADIUS}" fill="${c.bg}" stroke="${c.border}"/>
  <path d="M0.5 ${BAR}V${RADIUS}a${RADIUS} ${RADIUS} 0 0 1 ${RADIUS}-${RADIUS}h${w - 1 - RADIUS * 2}a${RADIUS} ${RADIUS} 0 0 1 ${RADIUS} ${RADIUS}v${BAR - RADIUS}z" fill="${c.bar}"/>
  <line x1="0.5" y1="${BAR}" x2="${w - 0.5}" y2="${BAR}" stroke="${c.border}"/>
  <circle cx="20" cy="${BAR / 2}" r="5.5" fill="${c.dots[0]}"/>
  <circle cx="39" cy="${BAR / 2}" r="5.5" fill="${c.dots[1]}"/>
  <circle cx="58" cy="${BAR / 2}" r="5.5" fill="${c.dots[2]}"/>
  <text x="${w / 2}" y="${BAR / 2 + 4}" text-anchor="middle" font-size="12" fill="${c.title}">${esc(title)}</text>
  <g>
${lines}
  </g>
</svg>
`
}
