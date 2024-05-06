import { Tx, UTxO } from 'translucent-cardano'
//import { StateHolderStateHolder } from '../configs/plutus'
import { StateHolderStateHolder, Val } from '@adatag/common/plutus'
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
  fetchDatum(txHash: string, address: string): Promise<{ output_index: number; datum: string } | undefined>
  fetchTxUtxos(transaction_id: string): Promise<TransactionOutput[]>
  fetchAsset(adatag: string, oldestFirst?: boolean, unspent?: boolean): Promise<TransactionOutput | undefined>
  setSyncStateCallback(callback: SyncStateCallback): void
  onSyncStatusChange?: SyncStateCallback
}

export type TreeState = StateHolderStateHolder['oldState']

export interface MintingService {
  buildBaseTx(adatag: string, useAdaHandle: boolean, userAddress: string | undefined, deposit: bigint): Promise<Tx>
  finaliseTx(tx: Tx, adatag: string, userAddress: string | undefined, datum: string, redeemer: string): Promise<Tx>
}

export interface ClaimingService {
  buildClaimTx(action: 'Collect' | 'Redeem', beneficiary: string, donation: bigint, depositUtxos: UTxO[]): Promise<Tx>
}

export type TransactionDetails = {
  transaction_index: number
  transaction_id: string
  output_index: number
  address: string
  value: {
    coins: number
    assets: { [key: string]: number }
  }
  datum_hash: null | string
  script_hash: null | string
  created_at: {
    slot_no: number
    header_hash: string
  }
  spent_at: null | {
    slot_no: number
    header_hash: string
  }
}

export type TransactionOutput = {
  transaction_index: number
  transaction_id: string
  output_index: number
  address: string
  value: {
    coins: number
    assets: { [policyId: string]: number }
  }
  datum_hash: string | null
  datum_type: string | null
  script_hash: string | null
  created_at: {
    slot_no: number
    header_hash: string
  }
  spent_at: {
    slot_no: number
    header_hash: string
  } | null
}
