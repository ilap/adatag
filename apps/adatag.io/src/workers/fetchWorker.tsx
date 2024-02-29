import * as Comlink from 'comlink'

import {
  SERVER_URL,
  BOOTSTRAP_SLOT,
  SAFETY_SLOTS,
  POLICY_ID,
} from './constants'
import { hexToASCII } from '../utils'
import { FetchWorkerApi, QueryWorkerApi } from './types'
import { JSONParser } from '@streamparser/json'

class FetchWorker implements FetchWorkerApi {
  db: QueryWorkerApi | undefined

  async init(db: QueryWorkerApi): Promise<any> {
    console.log('%%%%%%%%%%%%%%%%%%%%%%%%%%%% Called....')
    this.db = db
    return true
  }

  /* FIXME: async  #parseResponseStream(response: Response, parser: JSONParser) {
    const reader = response.body?.getReader();
    while (true) {
      // @ts-ignore:next-line
      const { done, value } = await reader!.read();
      if (done) break;
      parser.write(value);
    }
  }
*/

  async fetchData(): Promise<void> {
    try {
      const tip = await this.fetchTip()
      const before = tip.slot_no - SAFETY_SLOTS

      // Construct query parameters
      const queryParams = [
        'order=oldest_first',
        `created_after=${BOOTSTRAP_SLOT}`,
        `created_before=${before}`,
      ].join('&')

      // Construct URL for fetching matches
      const url = `${SERVER_URL}/matches/${POLICY_ID}.*?${queryParams}`

      // Fetch matches from the blockchain
      const response = await fetch(url)

      const parser = new JSONParser({
        stringBufferSize: undefined,
        paths: ['$.*'],
      })

      parser.onValue = chunk => {
        // @ts-ignore:next-line
        // ...
        const { value } = chunk.value
        const data = value.assets
        let keysStartingWithPrefix = Object.keys(data).filter(key =>
          key.startsWith(POLICY_ID),
        )
        let tokenName = keysStartingWithPrefix.map(key => key.split('.')[1])[0]
        const assetName = hexToASCII(tokenName)
        console.log(`@@@@ ASSSETNAME: ${assetName}`)
      }

      const reader = response.body?.getReader()

      async function processStream() {
        while (true) {
          const { done, value } = await reader!.read()
          if (done) break
          parser.write(value)
        }
      }

      //await this.#parseResponseStream(response, parser)
      await processStream()

      return
    } catch (error) {
      console.error('Error fetching data:', error)
      return
    }
  }

  async fetchTip(): Promise<any> {
    const response = await fetch(`${SERVER_URL}/checkpoints`)
    const checkpoints = await response.json()
    return checkpoints[0]
  }

  async fetchMetadata(slotNo: number, transactionId: string): Promise<any> {
    return slotNo
  }

  async warnIfNotSynchronized(): Promise<any> {
    return false
  }
}

const workerInstance = new FetchWorker()

Comlink.expose(workerInstance)
