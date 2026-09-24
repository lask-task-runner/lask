import { cleanup } from '@testing-library/react'
import { afterEach } from 'vitest'
import '@testing-library/jest-dom/vitest'

// @testing-library/react's auto-cleanup only self-registers when it detects
// a global `afterEach` (i.e. `test.globals: true` in vite.config.js). Since
// this project keeps vitest's globals explicit/imported instead, unmount
// after every test here so state doesn't leak between them.
afterEach(() => {
  cleanup()
})
