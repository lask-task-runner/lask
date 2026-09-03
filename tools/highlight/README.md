# Highlight renderer

GitHub does not know the `lask` language, and its Markdown sanitizer strips `<style>`
and `style=` attributes, so a code fence in `README.md` cannot be coloured. Instead the
README embeds pre-rendered SVG "code cards" that carry their colours as `fill`
attributes, which GitHub does render.

Two cards are built from the same chrome, one per theme (`prefers-color-scheme` picks
between them in the README's `<picture>` element):

| Card | Source | Renderer |
| ---- | ------ | -------- |
| the example module | `doc/assets/main.lask` | `render.mjs` |
| what running it prints | `doc/assets/session.txt` | `render-session.mjs` |

- `lask.tmLanguage.json` — TextMate grammar for Lask (comments, reserved words, types,
  keyword parameters, command strings `$`/`$2`/`$*`, environments `#rancher/cowsay`, and
  `#{...}` interpolation).
- `render.mjs` — tokenizes Lask with [Shiki](https://shiki.style) using that grammar.
- `render-session.mjs` — colours a captured transcript: the lines you typed, the
  timestamps, the `[#env]` tags, the `1|`/`2|` stream markers, and the exit status.
- `card.mjs` — the shared SVG card. Every glyph is placed on the monospace grid with its
  own `x`, so the layout survives whatever monospace font the reader's machine picks.

## Regenerate

```bash
$ cd tools/highlight
$ npm install
$ npm run capture     # re-runs the example, rewrites doc/assets/session.txt
$ npm run render      # writes doc/assets/{main,session}-{dark,light}.svg
```

`npm run capture` needs `lask` on the `PATH` and a running Docker daemon. It drops one
line from the transcript: Docker's `WARNING: The requested image's platform ...`, which
only appears when the amd64 `rancher/cowsay` image runs on an arm64 host. Everything else
is the real output, timestamps included.

Both renderers take `<source> <out-dir> [title]`, so they work for other snippets too.

## When editing the example

`doc/assets/main.lask` is the single source for the README hero. After changing it,
re-capture, re-render, and paste the same text into the `<details>` fence in `README.md`,
which exists so the example stays copy-pastable and searchable.

The `<img width="...">` of each `<picture>` in `README.md` must match that SVG's own
`width` (printed in its first line), otherwise GitHub scales the card and the text goes
soft.
