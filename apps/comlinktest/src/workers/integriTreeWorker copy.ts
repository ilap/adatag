import { ChainFetch } from "../services/ChainFetch.ts";
import { SQLiteDataStore as DataStore } from "../services/SQLiteDataStore.ts";
import { ChainFetchService, DataStoreService, SyncState } from "../services/types.ts";
import { hexToASCII, stringifyData } from "../utils.ts";
import * as Comlink from 'comlink'
import { IntegriTree } from '../utils/integri-tree.ts'
import { Val } from '../utils/plutus_types.ts'

import { Data } from 'translucent-cardano'
import * as P from '../utils/plutus.ts'


export interface WorkerContextData {
  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void
  init(): Promise<void>
  stopFetching(): Promise<void>
  buildTree(newAdatag: string): Promise<any>
}

//import { WorkerContextData } from "./types.ts";

export class IntegriTreeWorker implements WorkerContextData {
  chainFetch: ChainFetch;
  dataStore: DataStore

  constructor() {
    this.dataStore = new DataStore();
    this.chainFetch = new ChainFetch(this.dataStore);
  }

  setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void {
    this.chainFetch.onStatusChange = callback;
    //this.dataStore.onStatusChange = callback;
  }
  
  
  async init(): Promise<void> {
    console.log(`###$$$%%%% START FETCHING IN CLASS`)
    setInterval(async () => {
      // Fetch data from the network starting from last @adatag created or
      // after the processed slots. Initial fetch startes from bootstrap time.
      await this.chainFetch.fetchDataChunks()
    }, 5000)
  }

  async stopFetching() {
    this.chainFetch.stopFetching();
  }

  async buildTree(newAdatag: string): Promise<any> {
    return
  }

}

const workerInstance = new IntegriTreeWorker();
Comlink.expose(workerInstance);