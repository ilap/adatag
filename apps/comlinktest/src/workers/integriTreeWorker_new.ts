import * as Comlink from 'comlink'

import { ChainFetchService as FetchService, DataStoreService, SyncState } from "../services/types.ts";
import { SQLiteDataStore } from '../services/SQLiteDataStore.ts';
import { ChainFetch as Fetch } from '../services/ChainFetch.ts';

export interface WorkerContextData {
  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void
  init(): Promise<void>
  stopFetching(): Promise<void>
  buildTree(newAdatag: string): Promise<any>
}

export class WorkerContext implements WorkerContextData {
  fetch: FetchService;
  dataStore: DataStoreService

  constructor() {
    this.dataStore = new SQLiteDataStore();
    this.fetch = new Fetch(this.dataStore);
  }

  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void {
    this.fetch.onStatusChange = callback;
  }

  async init(): Promise<void> {
    console.log(`### THIS IS NOT CALLED`)
    this.fetch.startFetching()
  }

  async stopFetching() {
    this.fetch.stopFetching();
  }

  async buildTree(newAdatag: string): Promise<any> {
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
/*
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

      if (hexToASCII(oldState!.adatag) != lastVal) {
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

    // Append adatag and generate its root hash
    //tree.append(adatag)
    //const newRootHash = tree.rootHash()
    // Query all elements from the DB
    const { updateVal, appendVal, proof } = tree.generateMinimalSubtree(newAdatag)!

    console.log(
      `############ NEWPROF ${JSON.stringify(updateVal)} ${JSON.stringify(appendVal)} ${JSON.stringify(proof)}`
    )*/
    return
  }

}

const workerInstance = new WorkerContext();
Comlink.expose(workerInstance);