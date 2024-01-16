import { test, expect } from 'bun:test'
import { emptyHash, hash256 } from './hash'
import * as shared from '@adatag/shared/config'

test('Hash check', async () => {
  const eh = hash256('')
  expect(eh == emptyHash)
})
