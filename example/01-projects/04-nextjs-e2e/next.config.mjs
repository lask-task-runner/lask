// `lask run dev` edits files on the host and serves them from a container
// that sees them through a bind mount. Not every Docker setup passes
// file-change events across that mount, so the dev server polls instead.
// Only `next dev` watches files; a build is unaffected.
const nextConfig = {
  watchOptions: { pollIntervalMs: 1000 },
};

export default nextConfig;
