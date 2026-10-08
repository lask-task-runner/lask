A Go program released the way supply-chain guidance now asks for: built for four platforms, described by a software bill of materials (SBOM), checked for known vulnerabilities, and signed, with one command. Go, Syft, Grype and cosign all run in containers, so none of them is installed on your machine, and CI runs exactly the same steps.

## Try it right now

You need **Lask** and **Docker**:

```bash
cd example/01-projects/05-go-supply-chain
lask sync                                          # one-time, needs network: tools module and images
export COSIGN_PASSWORD='choose-a-passphrase'
lask run keygen                                    # one-time: writes cosign.key and cosign.pub
lask eval release --version v1.0.0
```

```json
["dist/hello_v1.0.0_linux_amd64","dist/hello_v1.0.0_linux_arm64","dist/hello_v1.0.0_darwin_arm64",
 "dist/hello_v1.0.0_windows_amd64.exe","dist/hello_v1.0.0.spdx.json","dist/checksums.txt","dist/checksums.txt.bundle"]
```

The binaries are real, so run the one for your machine: `./dist/hello_v1.0.0_darwin_arm64 --name Lask`.

The first release takes a couple of minutes, mostly Grype downloading its vulnerability database. The database and Go's caches are kept in Docker volumes (`lask-grype-cache`, `lask-go-cache`), so later runs are quick.

`cosign.key` must stay private; `cosign.pub` is the half you would publish. This example's [.gitignore](.gitignore) leaves out both, so each reader makes their own.

## What `release` does

[main.lask](main.lask) runs these steps in order. Each one is also a task you can run on its own:

| Step | Task | Runs in |
| --- | --- | --- |
| `go vet` and `go test`, at the same time | `test` | `golang` |
| Build for linux/amd64, linux/arm64, darwin/arm64 and windows/amd64, all at once | `build_all`, or `build <os> <arch>` | `golang`, one container per platform |
| Write an SPDX SBOM from what Go embedded in each binary | `sbom` | Syft |
| Fail on any vulnerability of `--fail-on` severity or worse (default `high`), while the next two steps run | `scan <sbom>` | Grype |
| Write `checksums.txt` over every file | `checksums` | coreutils |
| Sign `checksums.txt`, and so everything it lists | `sign` | cosign |
| Check the signature, then every checksum | `verify` | cosign, coreutils |

The cross-compilation target belongs to the environment, not the command. `build` asks the tools module for a Go environment with GOOS and GOARCH set, and `build_all` starts one per platform with `async`:

```lask
build(os: String, arch: String, --version: String = "dev"): String = do {
  ext = if (os == "windows") { ".exe" } else { "" }
  out = "dist/hello_#{version}_#{os}_#{arch}#{ext}"
  target = tools.go(goos = os, goarch = arch, cgo_enabled = "0", cache_dir = go_cache)
  $[target] go build -trimpath -ldflags "-s -w -X main.version=#{version}" -o #{out} ./cmd/hello
  out
}

build_all(--version: String = "dev"): Array<String> =
  all(map(platforms, \(p: Platform) -> async build(p.os, p.arch, version = version)))
```

The scan does not hold up signing. It is started with `async` and awaited before `verify`, so a vulnerability still fails the release:

```lask
scanned = async scan(sbom_file, fail_on = fail_on)
checksums()
sign(password = password)
await scanned
```

The key's password is a `!!` parameter, read from `COSIGN_PASSWORD` by default. It reaches cosign only through the container's environment, and Lask prints it as `***` wherever it would appear in the log.

## The images

Every image is pinned in [lask.lock.json](lask.lock.json). Go, Syft, Grype and coreutils come from [lask-module-tools](https://github.com/lask-task-runner/lask-module-tools), declared in [lask.json](lask.json). cosign's official image has no shell for a command to run in, so this project builds its own from a short recipe, [tools/cosign.Dockerfile](tools/cosign.Dockerfile). The recipe is named where the environment is written:

```lask
cosign_image = #./tools/cosign.Dockerfile
```

`lask sync` builds it. The lock pins it by the hash of the recipe, so an edited Dockerfile is found the next time `lask sync --frozen` runs in CI.

## Signing without a transparency log

To run offline and without an account, `sign` keeps the signature local (`--tlog-upload=false`), and `verify` accordingly passes `--insecure-ignore-tlog=true`. A public project would normally sign keyless through Sigstore, where the signature is recorded in the Rekor transparency log and tied to a CI identity instead of a key file. That change is confined to the `sign` and `verify` tasks.

## Check it without running anything

```bash
lask check          # names, arguments and types; nothing runs
lask envs list      # every image, what requires it, and its pin
lask run --help     # every task, from its own comments
```
