A local large language model, driven entirely by Lask tasks: pull a model, ask it a question from the command line, compare several models side by side, or chat with it in the browser. Everything runs in containers on your own machine, and the containers that answer questions have no network at all, so no prompt can leave it.

## Try it right now

You need **Lask** and **Docker**. No Ollama, no Python, no GPU, no account:

```bash
cd example/01-projects/03-local-llm
lask sync                              # one-time, needs network: pulls the two images
lask run pull                          # one-time, needs network: downloads a ~400 MB model
lask eval ask "Write a haiku about task runners"
```

`lask sync` and `lask run pull` are the only steps that touch the network: `ask`, `models` and `compare` run with `network: "none"`, so a model that was never pulled fails at once instead of downloading quietly. The model lands in a Docker volume (`lask-ollama-models`), so it is downloaded once; `docker volume rm lask-ollama-models` gives the space back.

The default model, `qwen2.5:0.5b`, is small enough for a laptop without a GPU. For better answers, pick another from [the Ollama library](https://ollama.com/library):

```bash
lask run pull --model llama3.2:3b
lask eval ask "Explain a monad in one sentence" --model llama3.2:3b
```

## What the tasks look like

An environment is a value, so where the network is allowed is written once, at the top of [main.lask](main.lask), rather than remembered at every call:

```lask
ollama_online = #ollama/ollama:0.12.5{volumes: [models_volume]}
ollama_offline = #ollama/ollama:0.12.5{volumes: [models_volume], network: "none"}
```

`pull` runs on the first, everything else on the second. Each task starts a throwaway Ollama server in its container, runs one `ollama` command next to it, and the server goes when the container does.

`ask` returns the answer as a value, so `eval` can hand it to the next program. The prompt goes through `shell_quote`, so quotes, `$(...)` or `;` in it reach the model as text:

```bash
lask eval --stdout-encode text ask "Name three colours"
```

A prompt that itself starts with `--` would be taken for an option by Lask's own command line, so pass it as a JSON string: `lask eval ask '"--help me write a CLI flag"'`.

`compare` starts one container per model with `async` and joins them with `all`, returning a typed list:

```bash
lask run pull --model llama3.2:1b
lask eval compare "Explain a monad in one sentence" qwen2.5:0.5b llama3.2:1b
```

```json
[{"answer":"...","model":"qwen2.5:0.5b"},{"answer":"...","model":"llama3.2:1b"}]
```

## Chat in the browser

```bash
lask run serve
```

opens [http://localhost:3000](http://localhost:3000) (the first start takes a minute): Open WebUI, in its image that has Ollama built in, on the same volume of models. It pulls the model first if it is missing.

Sign-in is switched off so the page works at once, which is why the port is published on `127.0.0.1` only: the page is reachable from this machine and nothing else. Open WebUI keeps its settings and chat history in a second volume, `lask-open-webui`, and downloads a small embedding model into it on its first start.

> **Stopping.** Interrupting `lask run serve` (Ctrl-C) currently leaves the container running, holding port 3000. Stop it with `docker stop $(docker ps -q --filter ancestor=ghcr.io/open-webui/open-webui:v0.6.34-ollama)`; it removes itself once stopped, and `serve` then ends with the container's exit code.

## Check it without running anything

```bash
lask check          # names, arguments and types; no image is pulled
lask envs list      # the two images, and whether each is pinned
lask run --help     # every task, from its own comments
```
