// TreeService.ts
import { Val, IntegriTree } from '@adatag/integri-tree'
import { Data } from 'translucent-cardano'
import * as P from '../utils/plutus.ts'

import { hexToASCII, stringifyData } from '../utils.ts'

import { ChainFetchService, DataStoreService, SyncState } from "../services/types.ts";

import { SQLiteDataStore as DataStore } from "../services/SQLiteDataStore.ts";
import { KupmiosChainFetch as ChainFetch } from '../services/KupmiosChainFetch.ts';

export interface TreeService {
  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void
  init(): Promise<void>
  stopFetching(): Promise<void>
  buildTree(newAdatag: string): Promise<any>
}

export class IntegriTreeWorker implements TreeService {
  private chainFetch: ChainFetchService;
  private dataStore: DataStoreService
  private intervalId?: Timer | null = null

  constructor() {
    this.dataStore = DataStore.getInstance();
    this.chainFetch = new ChainFetch(this.dataStore);
  }

  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void {
    this.chainFetch.onStatusChange = callback;
  }

  async init(): Promise<void> {
    console.log(`Starting fetch data.`)
    // Start fetching data and store the interval ID
    this.intervalId = setInterval(async () => {
      // Fetch data from the network starting from last @adatag created or
      // after the processed slots. Initial fetch starts from bootstrap time.
      await this.chainFetch.fetchDataChunks();
    }, 5000);
  }

  async stopFetching(): Promise<void> {
    // Check if intervalId is not null (meaning setInterval has been called before)
    if (this.intervalId) {
      console.log("Stopping fetch data.")
      clearInterval(this.intervalId);
      this.intervalId = null;
    } else {
      console.log("Fetching is not currently active.");
    }
  }

  // It builds the tree from the data retrieved from the database and the additionaly fetched elements (delta)
  async buildTree(newAdatag: string): Promise<void> {
    // Gather the latest state of the IntegriTree stored on chain.
    const authTokenName = newAdatag[0]

    const { slot, datum } = await this.chainFetch.fetchStateDatum(authTokenName)

    const oldState = Data.from(datum!, P.StateHolderStateHolder.oldState)

    console.log(`@@@@ HEUREKA OLD STATE:  ${stringifyData(oldState)}`)

    // Gather all the Vals from the database
    const [valSlot, elements] = await this.dataStore.getAllVal(authTokenName)

    console.log(`### ELEMENTS ${slot} ${JSON.stringify(elements)}`)

    // Fetch the missing elements and append into the tree if required
    const tree = IntegriTree.fromList(elements)

    // Get all missing elements if there is any
    //const tip = await this.chainService.fetchTip()
    // FIXME: It does not work
    console.warn(`#################### AAAAAAAA : ${valSlot}`)
    const elems = (await this.chainFetch.fetchElements(authTokenName, valSlot, null))
      .filter((str: string) => str[0] == newAdatag[0])

    if (elems.length > 0) {
      console.log(`@@ ELEMS ${JSON.stringify(elems)}`)

      const lastVal: Val = elems[elems.length - 1]
      console.log(`@@ LASTVAL ${JSON.stringify(lastVal)} ..... ${stringifyData(oldState)}`)

      if (hexToASCII(oldState.adatag) != lastVal) {
        // inconsistent tree: ERROR & HALT.
        throw Error(`inconsistent tree: ERROR & HALT ${oldState.adatag} ... ${lastVal}`)
      }

      elems.forEach(elem => {
        console.log(`#### Append adatag to tree: ${elem} `)
        tree.append(elem)
      })
    }

    // The the built tree's proof
    const treeProof = tree.rootHash()

    if (oldState.rootHash != treeProof) {
      throw Error(`inconsistent tree: ERROR & HALT ${oldState.rootHash} ${treeProof}`)
    } else {
      console.log(`@@@@@@ HEUREKA: The tree is consistent.... with ${treeProof}`)
    }

    const { updateVal, appendVal, proof } = tree.generateMinimalSubtree(newAdatag)!

    console.log(
      `############ NEWPROF ${JSON.stringify(updateVal)} ${JSON.stringify(appendVal)} ${JSON.stringify(proof)}`
    )
  }
}

export default TreeService
