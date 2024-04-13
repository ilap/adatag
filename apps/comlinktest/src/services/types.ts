import { Val } from '@adatag/integri-tree'

export  type { Val } 

export type State = "idle" | "syncing" | "synced" | "error";

export type SyncState = {
    state: State;
    message: string;
}

export interface ChainData {
  tokenName: string
  createdAt: number
}

// Data store service interface
export interface DataStoreService {
  updateDatabase(data: ChainData): Promise<void>
  getLastSlot(): Promise<number>
  setLastSlot(slot: number): Promise<void>
  getAllVal(tableName: string): Promise<[number, Val[]]>
}


export type TreeState = {
    // state change slot i.e. auth tokens' eUTxO creation slot.
    slot: number

    // The datum retrieved from the auth token's datum
    datum: string
  }
  
  // Chain service interface
  export interface ChainFetchService {
    fetchTip(): Promise<number>
    fetchDataChunks(): Promise<void>
    fetchStateDatum(authNft: string): Promise<TreeState>
    fetchElements(adatag: string, fromSlot: number, toSlot: number | null): Promise<string[]>
    onStatusChange?: ((newState: SyncState) => void)| null;
  }
