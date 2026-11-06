# lask-egress-proxy

The proxy a container given `egress` reaches the network through (lask spec 10.12). Lask starts it on two networks: one with no route out, where the container is, and one with a route out. It forwards a connection only to a destination its arguments list, and refuses every other.

```text
lask-egress-proxy [-listen :3128] <host>:<port> ...
```

- `CONNECT <host>:<port>` is tunnelled, for HTTPS and any other protocol over TCP. A plain HTTP request is forwarded by its host and port.
- A name is resolved by the proxy, and the address it resolved to is the one connected to. Loopback, link-local, multicast and unspecified addresses are refused unless an argument names the address.
- A refused destination is answered `403`. Each refusal is a line of JSON on standard output, `{"event":"denied","destination":"<host>:<port>","reason":"..."}`, which Lask reads; `{"event":"ready"}` is written once it listens.

It is part of Lask's repository so that the entries, their meaning and the log Lask reads change with Lask in one commit. The image is built and published by the `egress-proxy` workflow, and the digest a release runs is written in `src/Language/Lask/Runtime/EgressProxy.hs`.

```sh
go test ./...
docker build -t lask-egress-proxy:dev .
```
