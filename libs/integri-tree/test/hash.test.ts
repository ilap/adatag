import { test, expect } from 'bun:test'
import { emptyHash, hash256 } from '../src/lib/hash'

test('Hash check', async () => {
  const eh = hash256('')
  expect(eh == emptyHash)
})
