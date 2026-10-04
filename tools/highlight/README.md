# Highlight renderer

GitHub does not know the `lask` language, and its Markdown sanitizer strips `<style>` and `style=` attributes, so a code fence in `README.md` cannot be coloured. Instead the README embeds a pre-rendered SVG "code card" that carries its colours as `fill` attributes, which GitHub does render — one per theme, selected by `prefers-color-scheme` in the README's `<picture>` element.

- `lask.tmLanguage.json` — TextMate grammar for Lask (comments, reserved words, types, keyword parameters, command strings `$`/`$2`/`$*`, environments `#golang:1.22`, and `#{...}` interpolation).
- `render.mjs` — tokenizes `doc/assets/example.lask` with [Shiki](https://shiki.style) using that grammar.
- `card.mjs` — the shared SVG card. Every glyph is placed on the monospace grid with its own `x`, so the layout survives whatever monospace font the reader's machine picks.
- `render-session.mjs` — colours a captured terminal transcript instead of source: the lines you typed, timestamps, `[#env]` tags, `1|`/`2|` stream markers, exit status, and `E-*` error codes. Nothing in the README uses it right now.

## Regenerate

```bash
$ cd tools/highlight
$ npm install
$ npm run render      # writes doc/assets/example-{dark,light}.svg
```

Both renderers take `<source> <out-dir> [title]` and write `<source name>-{dark,light}.svg`, so they work for other snippets too.

## When editing the excerpt

`doc/assets/example.lask` is an abridged excerpt of `example/01-projects/02-webapp-on-aws/main.lask` (some lines and comments are shortened or left out); the README links to the full file below it. After changing either, re-render.

The `<img width="...">` of the `<picture>` in `README.md` must match the SVG's own `width` (printed in its first line), otherwise GitHub scales the card and the text goes soft.
