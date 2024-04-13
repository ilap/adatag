import { Val } from "../utils/types";

export type SyncState = "idle" | "syncing" | "synced" | "error";

export type StateDetails = {
  // state change slot i.e. auth tokens' eUTxO creation slot.
  slot: number;
  // The datum retrieved from the auth token's datum
  datum: string;
};

export interface ChainFetchService {
  onStatusChange: ((newState: SyncState) => void)| null;
  startFetching(): Promise<void>;
  stopFetching(): void;
  fetchTip(): Promise<number>;
  fetchDataChunks(): Promise<void>;
  fetchStateDatum(authNft: string): Promise<StateDetails>;
  fetchElements(
    adatag: string,
    fromSlot: number,
    toSlot: number | null
  ): Promise<string[]>;
}

export interface ChainData {
  tokenName: string;
  createdAt: number;
}

export interface DataStoreService {
  updateDatabase(data: ChainData): Promise<void>;
  getLastSlot(): Promise<number>;
  setLastSlot(slot: number): Promise<void>;
  getAllVal(tableName: string): Promise<[number, Val[]]>;
}
