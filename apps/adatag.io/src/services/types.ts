import { UTxO, Tx, TxComplete, Translucent } from 'translucent-cardano'
import { IntegriTree } from '../../../../libs/integri-tree/src/lib/integri-tree.ts'
import { StateHolderStateHolder } from '../configs/plutus.ts'
import { MintRedeemer, Val } from '../configs/types.ts' // FIXME: '@adatag/integri-tree'
export type { Val }

export type State = 'idle' | 'syncing' | 'synced' | 'error'

export type SyncState = {
  state: State
  message: string
}

export type SyncStateCallback = ((newState: SyncState) => void) | null

export interface ChainData {
  tokenName: string
  createdAt: number
}

export interface Initialisable {
  initialise(): Promise<void>
}

// Data store service interface
export interface DataStoreService extends Initialisable {
  updateDatabase(data: ChainData): Promise<void>
  getLastSlot(): Promise<number>
  setLastSlot(slot: number): Promise<void>
  getAllVal(tableName: string): Promise<[number, Val[]]>

  cleanup(): Promise<void>
}

export type TreeDetails = {
  // state change slot i.e. auth tokens' eUTxO creation slot.
  slot: number

  // The datum retrieved from the auth token's datum
  datum: string
}

// Chain service interface
export interface ChainFetchService extends Initialisable {
  fetchTip(): Promise<number>
  fetchAndSaveElements(): Promise<void>
  fetchElements(fromSlot: number, toSlot: number): Promise<string[]>
  fetchStateDatum(authNft: string): Promise<TreeDetails | null>
  fetchAsset(adatag: string): Promise<boolean>
  setSyncStateCallback(callback: SyncStateCallback): void
  onSyncStatusChange?: SyncStateCallback
}

export type TreeState = StateHolderStateHolder['oldState']

export interface MintingService {
  buildBaseTx(
    adatag: string,
    useAdaHandle: boolean,
    userAddress: string | undefined,
    deposit: bigint,
  ): Promise<Tx>
  finaliseTx(
    tx: Tx,
    adatag: string,
    userAddress: string | undefined,
    datum: string,
    redeemer: string,
  ): Promise<Tx>
}
