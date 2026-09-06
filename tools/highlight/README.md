# Highlight renderer

GitHub does not know the `lask` language, and its Markdown sanitizer strips `<style>`
and `style=` attributes, so a code fence in `README.md` cannot be coloured. Instead the
README embeds a pre-rendered SVG "code card" that carries its colours as `fill`
attributes, which GitHub does render — one per theme, selected by `prefers-color-scheme`
in the README's `<picture>` element.

- `lask.tmLanguage.json` — TextMate grammar for Lask (comments, reserved words, types,
  keyword parameters, command strings `$`/`$2`/`$*`, environments `#golang:1.22`, and
  `#{...}` interpolation).
- `render.mjs` — tokenizes `doc/assets/main.lask` with [Shiki](https://shiki.style) using
  that grammar.
- `card.mjs` — the shared SVG card. Every glyph is placed on the monospace grid with its
  own `x`, so the layout survives whatever monospace font the reader's machine picks.
- `render-session.mjs` — colours a captured terminal transcript instead of source: the
  lines you typed, timestamps, `[#env]` tags, `1|`/`2|` stream markers, exit status, and
  `E-*` error codes. Nothing in the README uses it right now; it is kept for when a
  transcript card is worth showing again.

## Regenerate

```bash
$ cd tools/highlight
$ npm install
$ npm run render      # writes doc/assets/main-{dark,light}.svg
```

Both renderers take `<source> <out-dir> [title]`, so they work for other snippets too.

## When editing the example

`doc/assets/main.lask` is the single source for the README hero. After changing it,
re-render and paste the same text into the `<details>` fence in `README.md`, which exists
so the example stays copy-pastable and searchable.

The `<img width="...">` of the `<picture>` in `README.md` must match the SVG's own `width`
(printed in its first line), otherwise GitHub scales the card and the text goes soft.
