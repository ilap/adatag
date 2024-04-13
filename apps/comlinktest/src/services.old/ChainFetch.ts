import { ChainData, DataStoreService, ChainFetchService, StateDetails, SyncState } from './types'

import axios from "axios"
// FIXME USE AXIOS
/*const response = await axios.get(
  "https://jsonplaceholder.typicode.com/posts",
);
const posts = response.data;
*/

import { JSONParser } from '@streamparser/json'
import { hexToASCII } from '../utils.ts'

import { SAFETY_SLOTS } from './config.ts'

// Continuous Fetching Worker
export class ChainFetch implements ChainFetchService {
  private dataStore: DataStoreService;
  private fetchInterval: number;
  private fetchIntervalId: NodeJS.Timeout | null;

  onStatusChange: ((newState: SyncState) => void)| null = null;

  constructor(dataStore: DataStoreService, fetchInterval = 60000) {
    console.log(`Initialising ChainFetch`)
    this.dataStore = dataStore;
    this.fetchInterval = fetchInterval;
    this.fetchIntervalId = null;
    //this.startFetching()
  }


  private getAdatagFromAssets(assets: any, policyId: string): string {
    const keysStartingWithPrefix = Object.keys(assets).filter(key => key.startsWith(policyId))
    const tokenHex = keysStartingWithPrefix.map(key => key.split('.')[1])[0]
    return hexToASCII(tokenHex)
  }

  // Function to parse response stream
  private async parseResponseStream(response, parser) {
    const reader = response.body.getReader()
    while (true) {
      const { done, value } = await reader.read()
      if (done) break
      parser.write(value)
    }
  }

  async startFetching(): Promise<void> {
    console.log(`###$$$%%%% START FETCHING IN CLASS`)
    setInterval(async () => {
      // Fetch data from the network starting from last @adatag created or
      // after the processed slots. Initial fetch startes from bootstrap time.
      await this.fetchDataChunks()
    }, 5000)
  }

  stopFetching(): void {
    this.onStatusChange?.('idle');
    if (this.fetchIntervalId) {
      clearTimeout(this.fetchIntervalId);
      this.fetchIntervalId = null;
    }
  }


  async fetchTip(): Promise<number> {
    const response = await fetch(`http://localhost:1442/checkpoints`)
    const checkpoints = await response.json()
    // Note: checkpoints are ordered by descending slots, so the first item is always the tip
    return checkpoints[0].slot_no
  }

  /*
    [{
      "transaction_index": 0,
      "transaction_id": "8b05f5d14b64159dcc51c9a512d5e5ea820ec5a3e2d0d3e93d2e5bdabec2610f",
      "output_index": 0,
      "address": "addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m",
      "value": {
        "coins": 1189560,
        "assets": {
          "807b5305cd5c1f6e4d829e6ea92bdb258396f9515f11a9db9b172f32.61646f7a74796a6e6d676474612e6c69": 1
        }
      },
      "datum_hash": null,
      "script_hash": null,
      "created_at": {
        "slot_no": 109,
        "header_hash": "ae186104029b163f217b94bd779fed0671a9b2f822a93c09af228a39d6553120"
      },
      "spent_at": null
    },
    ...
  ]
  */

  // Fetch data as stream from the chain as chunks
  async fetchDataChunks(): Promise<void> {
    try {
      this.onStatusChange?.('syncing');
      // Getting the slot nr. of the last @adatag created
      // and start fetching from the nex slot
      const from = (await this.dataStore.getLastSlot()) + 1

      // Get the tip and some
      const tip = await this.fetchTip()
      const to = tip - SAFETY_SLOTS

      console.log(`$$$$$ FROM: ${from} TO: ${to}`)
      // Construct query parameters
      const queryParams = [
        'order=oldest_first',
        `created_after=${from}`,
        `created_before=${to}`,
      ].join('&')

      // Construct URL for fetching matches
      const genesisConfig = await ConfigLoader.loadGenesisConfig()
      const policyId = genesisConfig.adatagMinting.policyId

      const url = `http://localhost:1442/matches/${policyId}.*?${queryParams}`
      console.warn(`####### UUURL: ${url}`)
      const response = await fetch(url)

      const parser = new JSONParser({ paths: ['$.*'] })

      parser.onValue = async chunk => {
        // @ts-ignore: next-line
        const { value, created_at } = chunk.value
        const adatag = this.getAdatagFromAssets(value.assets, policyId)

        const chunkOfData: ChainData = {
          tokenName: adatag,
          createdAt: created_at.slot_no,
        }
        await this.dataStore.updateDatabase(chunkOfData)
      }

      // Store the last processed slot at the end of processing as a checkpoint.
      parser.onEnd = async () => {
        console.log(`## parser is ended with slot: ${to}`)
        await this.dataStore.setLastSlot(to)
      }
      this.onStatusChange?.('synced')
      // Parse response stream
      await this.parseResponseStream(response, parser)
      // XXX: console.log(`@@@@ GETLASTDETAILS: ${JSON.stringify(await this.fetchStateDatum("a"))}`)
      // XXX: console.log(`@@@@ GETALLELEMS: ${JSON.stringify(await this.fetchElements("a", 0, 800))}`)
    } catch (error) {
      this.onStatusChange?.('error')
      console.error('Error:', error)
    }
  }

  async fetchElements(adatag: string, fromSlot: number, toSlot: number | null): Promise<string[]> {
    if (toSlot && toSlot <= fromSlot) {
      return []
    }
    return new Promise(async (resolve, reject) => {
      try {
        const params = ['order=oldest_first', `created_after=${fromSlot}`]

        if (toSlot) {
          params.push(`created_before=${toSlot}`)
        }

        const queryParams = params.join('&')

        // Construct URL for fetching matches
        const genesisConfig = await ConfigLoader.loadGenesisConfig()
        const policyId = genesisConfig.adatagMinting.policyId

        const url = `http://localhost:1442/matches/${policyId}.*?${queryParams}`
        console.log(`### URL: ${url}`)
        const response = await fetch(url)

        const parser = new JSONParser({ paths: ['$.*'] })

        const adatags: string[] = []

        parser.onValue = async chunk => {
          // @ts-ignore: next-line
          const { value } = chunk.value
          const adatag = this.getAdatagFromAssets(value.assets, policyId)

          adatags.push(adatag)
        }

        parser.onEnd = async () => {
          resolve(adatags)
        }

        parser.onError = error => {
          reject(error)
        }

        // Parse response stream
        await this.parseResponseStream(response, parser)
      } catch (error) {
        console.error('Error:', error)
      }
    })
  }

  // It fetches the last authorization token from the blockhain.
  async fetchStateDatum(authNft: string): Promise<StateDetails> {
    try {
      const code = authNft.charCodeAt(0).toString(16)


      // Construct URL for fetching matches
      const genesisConfig = await ConfigLoader.loadGenesisConfig()
      const authPolicyId = genesisConfig.authTokenScript.policyId

      const url = `http://localhost:1442/matches/${authPolicyId}.${code}?unspent`
      const response = await fetch(url)

      // only 1 entry as it's an NFT.
      const [eutxo] = await response.json()

      const assets = eutxo.value.assets
      console.log(`****** ${JSON.stringify(assets)}`)
      let asset = Object.keys(assets).filter(key => key.startsWith(authPolicyId))
      let token = asset.map(key => key.split('.')[1])[0]

      const slot = eutxo.created_at.slot_no

      if (eutxo.datum_type != 'inline') {
        throw Error(`Inconsistent eUTxO: datum is not presented`)
      }

      // Get the datum...
      const datRes = await fetch(`http://localhost:1442/datums/${eutxo.datum_hash}`)

      // only 1 entry as it's an NFT.
      const datumJson = await datRes.json()

      // XXX: console.log(`WWWWWWWW ${JSON.stringify(datumJson.datum)}`)
      const datum = datumJson.datum

      return { slot, datum }
    } catch (error) {
      console.error('Error:', error)
      throw Error()
    }
  }
}
