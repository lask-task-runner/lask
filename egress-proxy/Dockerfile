# The egress proxy (lask spec 10.12). Built for linux/amd64 and
# linux/arm64 and published as ghcr.io/lask-task-runner/lask-egress-proxy;
# each Lask release names the digest it runs.
FROM golang:1.26.2-alpine3.23@sha256:f85330846cde1e57ca9ec309382da3b8e6ae3ab943d2739500e08c86393a21b1 AS build
WORKDIR /src
COPY go.mod ./
COPY *.go ./
RUN go test ./... && CGO_ENABLED=0 go build -trimpath -ldflags="-s -w" -o /lask-egress-proxy .

FROM gcr.io/distroless/static-debian13:nonroot@sha256:25ac8f0c3b2334a9eb05452266bd4697864a8c8ef0ef71e5e6480d7f92868329
COPY --from=build /lask-egress-proxy /lask-egress-proxy
USER nonroot:nonroot
EXPOSE 3128
ENTRYPOINT ["/lask-egress-proxy"]
