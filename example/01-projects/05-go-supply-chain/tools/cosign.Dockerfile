# cosign's own image is distroless, with no shell for a Lask command to
# run in, so its binary is copied onto Alpine. Both images are pinned by
# their digests; the lock pins the result by the hash of this file.
FROM gcr.io/projectsigstore/cosign:v2.6.1@sha256:68839b7f13dac5a6744a5d8818e984dd39183374e37855c19e14d623d9bc9037 AS cosign
FROM alpine:3.22.2@sha256:4b7ce07002c69e8f3d704a9c5d6fd3053be500b7f1c69fc0d80990c2ad8dd412
COPY --from=cosign /ko-app/cosign /usr/local/bin/cosign
ENTRYPOINT []
