import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  test: {
    environment: 'jsdom',
    setupFiles: './src/setupTests.js',
    // Fixed values so App.test.jsx can assert against known URLs, instead
    // of the real ones `lask run deploy` injects at build time.
    env: {
      VITE_API_URL: 'https://example.test/',
      // Only the two the sign-out URL is built from; the rest of the OIDC
      // config is never exercised because react-oidc-context is mocked.
      VITE_COGNITO_DOMAIN: 'https://auth.example.test',
      VITE_COGNITO_CLIENT_ID: 'test-client-id',
      VITE_REDIRECT_URI: 'https://app.example.test/',
    },
  },
})
