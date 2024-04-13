import { Val } from '@adatag/integri-tree'
import { EventTarget } from 'event-target-shim';

// Define interfaces and types
export interface ChainData {
  tokenName: string
  createdAt: number
}



export type TreeStateDetails = {
  // state change slot i.e. auth tokens' eUTxO creation slot.
  slot: number

  // The datum retrieved from the auth token's datum
  datum: string
}


// Data store service interface
export interface DataStoreService {
//  updateDatabase(data: ChainData): Promise<void>
//  getLastSlot(): Promise<number>
//  setLastSlot(slot: number): Promise<void>
//  getAllVal(tableName: string): Promise<[number, Val[]]>
}


export type ChainStatus = 'error' | 'initialised' | 'syncing' | 'synced';

// Chain service interface
export interface ChainService {
  addEventListener(callback: EventTarget.FallbackEventListener): void //Promise<void>
//  fetchTip(): Promise<number>
//  fetchDataChunks(): Promise<void>
//  fetchStateDatum(authNft: string): Promise<TreeStateDetails>
//  fetchElements(adatag: string, fromSlot: number, toSlot: number): Promise<string[]>
}
