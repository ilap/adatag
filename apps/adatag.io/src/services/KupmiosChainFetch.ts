import { JSONParser } from '@streamparser/json'
import { SAFETY_SLOTS, KUPO_URL, FETCH_TIMEOUT } from '../configs/settings.ts'
import genesisConfig from '../configs/genesis-config.json'
import {
  ChainData,
  DataStoreService,
  ChainFetchService,
  SyncStateCallback,
  TreeState,
  TreeDetails,
} from './types.ts'
import { debugMessage, hexToASCII } from '../utils.ts'
import { fromText } from 'translucent-cardano'

export class KupmiosChainFetch implements ChainFetchService {
  private dataStore: DataStoreService
  onStatusChange?: SyncStateCallback
  private isFetching: boolean = false
  private policyId: string

  constructor(dataStore: DataStoreService) {
    this.dataStore = dataStore
    this.policyId = genesisConfig.adatagMinting.policyId
  }
  
  setSyncStateCallback(callback: SyncStateCallback): void {
    throw new Error('Method not implemented.')
  }
  onSyncStatusChange?: SyncStateCallback | undefined

  async initialise(): Promise<void> {
    //await this.dataStore.initialise()
    // fetch and save elements....
  }

  private getAdatagFromAssets(assets: any, policyId: string): string {
    const keysStartingWithPrefix = Object.keys(assets).filter(key =>
      key.startsWith(policyId),
    )
    const tokenHex = keysStartingWithPrefix.map(key => key.split('.')[1])[0]
    return hexToASCII(tokenHex)
  }

  private async fetchData(url: string): Promise<any[] | any> {
    const response = await fetch(url, { signal: AbortSignal.timeout(FETCH_TIMEOUT) });
    return await response.json()
  }

  private async fetchFromUrl(
    url: string,
    onData: (chunk: any) => void,
  ): Promise<void> {
    
      const controller = new AbortController();
      setTimeout(() => controller.abort(), 5000);
      const response = await fetch(url, { signal: controller.signal });

      if (!response.ok) {
        throw new Error(
          `Network error: ${response.status} - ${response.statusText}`,
        );
      }

      const parser = new JSONParser({ paths: ['$.*'] });
      parser.onValue = onData;

      await this.parseResponseStream(response, parser);
  }


  // Function to parse response stream
  private async parseResponseStream(response: Response, parser: JSONParser) {
    const reader = response.body?.getReader()
    while (true) {
      const { done, value } = await reader!.read()
      if (done) break
      parser.write(value)
    }
  }


  async fetchTip(): Promise<number> {
    const checkpoints = await this.fetchData(`${KUPO_URL}/checkpoints`)
    return checkpoints[0].slot_no
  }

  /**
   *
   *
   * ``` [{
   *   "transaction_index": 0,
   *   "transaction_id": "8b05f5d14b64159dcc51c9a512d5e5ea820ec5a3e2d0d3e93d2e5bdabec2610f",
   *   "output_index": 0,
   *   "address": "addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m",
   *   "value": {
   *     "coins": 1189560,
   *     "assets": {
   *       "807b5305cd5c1f6e4d829e6ea92bdb258396f9515f11a9db9b172f32.61646f7a74796a6e6d676474612e6c69": 1
   *     }
   *   },
   *   "datum_hash": null,
   *   "script_hash": null,
   *   "created_at": {
   *     "slot_no": 109,
   *     "header_hash": "ae186104029b163f217b94bd779fed0671a9b2f822a93c09af228a39d6553120"
   *   },
   *   "spent_at": null
   * },
   * ...
   * ]
   * ```
   */
  async fetchAndSaveElements(): Promise<void> {
    if (this.isFetching) {
      console.log(`fetchAndSaveElements is still fetching`)
      return // If already fetching, do nothing
    }
    this.isFetching = true

    try {
      console.warn(`#### BEFORE change`)
      this.onStatusChange?.({ state: 'syncing', message: 'Started fetching.' })
      let from = (await this.dataStore.getLastSlot()) + 1
      const tip = await this.fetchTip()
      console.warn(`### FROM ${from} ... TIP ${tip}`)
      const to = tip - SAFETY_SLOTS
      const queryParams = [
        `order=oldest_first`,
        `created_after=${from}`,
        `created_before=${to}`,
      ].join('&')
      // TODO: remove const policyId = genesisConfig.adatagMinting.policyId
      const url = `${KUPO_URL}/matches/${this.policyId}.*?${queryParams}`

      await this.fetchFromUrl(url, async (chunk: any) => {
        // @ts-ignore: next-line
        const { value, created_at } = chunk.value
        const adatag = this.getAdatagFromAssets(value.assets, this.policyId)

        const chunkOfData: ChainData = {
          tokenName: adatag,
          createdAt: created_at.slot_no,
        }
        this.onStatusChange?.({
          state: 'syncing',
          message: `Syncing. fetched adatag ${adatag}, from slot ${from++} of ${to}`,
        })
        await this.dataStore.updateDatabase(chunkOfData)
      })

      this.dataStore.setLastSlot(to)

      this.onStatusChange?.({
        state: 'synced',
        message: `Synced to slot ${to}, tip (${tip}), safety ${SAFETY_SLOTS}`,
      })
    } catch (error) {
      this.onStatusChange?.({ state: 'error', message: `${error}` })
    } finally {
      console.log(`fetchAndSaveElements is done`)
      this.isFetching = false
    }
  }

  async fetchElements(
    fromSlot: number,
    toSlot: number | null,
  ): Promise<string[]> {
    if (toSlot && toSlot <= fromSlot) {
      return []
    }

    const params = [`order=oldest_first`, `created_after=${fromSlot}`]
    if (toSlot && toSlot > fromSlot) {
      params.push(`created_before=${toSlot}`)
    }

    const queryParams = params.join('&')
    const policyId = genesisConfig.adatagMinting.policyId
    const url = `${KUPO_URL}/matches/${policyId}.*?${queryParams}`

    const adatags: string[] = []
    debugMessage(`#### URL: ${url}`)
    await this.fetchFromUrl(url, (chunk: any) => {
      const { value } = chunk.value
      const adatag = this.getAdatagFromAssets(value.assets, policyId)
      adatags.push(adatag)
    })

    return adatags
  }

  // It fetches the last authorization token from the blockhain.
  async fetchStateDatum(authNft: string): Promise<TreeDetails | null> {
    try {
      const authHex = fromText(authNft) //authNft.charCodeAt(0).toString(16)
      // Construct URL for fetching matches
      const authPolicyId = genesisConfig.authTokenScript.policyId
      const url = `${KUPO_URL}/matches/${authPolicyId}.${authHex}?unspent`

      const [eutxo] = await this.fetchData(url)
      const { created_at, datum_type, datum_hash } = eutxo

      const slot = created_at.slot_no

      if (datum_type != 'inline') {
        throw Error(`Inconsistent eUTxO: datum is not presented`)
      }

      // Get the datum...
      const datUrl = `${KUPO_URL}/datums/${datum_hash}`
      const { datum } = await this.fetchData(datUrl)

      return { slot, datum }
    } catch (error) {
      console.error('Error:', error)
      throw Error()
    }
  }

  async fetchAsset(adatag: string): Promise<boolean> {
    const hexStr = fromText(adatag)
    const url = `${KUPO_URL}/matches/${this.policyId}.${hexStr}?unspent`
    // It must be an NFT.
    try {
      const [asset] = await this.fetchData(url)
      debugMessage(`AAASSSEETT1: ${asset} ... ${url}`)
      return asset 
    } catch(e) {
      debugMessage(`AAASSSEETT2: ${(e as Error).name} ...`)
      throw e
    }
  }
}
