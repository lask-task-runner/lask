# Highlight renderer

GitHub does not know the `lask` language, and its Markdown sanitizer strips `<style>` and `style=` attributes, so a code fence in `README.md` cannot be coloured. These renderers work around that by drawing Lask source or a terminal transcript as an SVG "code card" that carries its colours as `fill` attributes, which GitHub does render.

Nothing in the README uses them right now: its hero is the recording in `doc/assets/lask-pv.gif` (built from `doc/assets/pv/`). They are kept for when a card is worth showing again.

- `lask.tmLanguage.json` — TextMate grammar for Lask (comments, reserved words, types, keyword parameters, command strings `$`/`$2`/`$*`, environments `#golang:1.22`, and `#{...}` interpolation).
- `render.mjs` — tokenizes a Lask source file with [Shiki](https://shiki.style) using that grammar.
- `card.mjs` — the shared SVG card. Every glyph is placed on the monospace grid with its own `x`, so the layout survives whatever monospace font the reader's machine picks.
- `render-session.mjs` — colours a captured terminal transcript instead of source: the lines you typed, timestamps, `[#env]` tags, `1|`/`2|` stream markers, exit status, and `E-*` error codes.

## Usage

```bash
$ cd tools/highlight
$ npm install
$ node render.mjs path/to/source.lask out-dir main.lask          # writes out-dir/source-{dark,light}.svg
$ node render-session.mjs path/to/transcript.txt out-dir title   # same, for a terminal transcript
```

Both renderers take `<source> <out-dir> [title]`. To embed a card, put the two SVGs in a `<picture>` selected by `prefers-color-scheme`, and give the `<img width="...">` the SVG's own `width` (printed in its first line); otherwise GitHub scales the card and the text goes soft.
