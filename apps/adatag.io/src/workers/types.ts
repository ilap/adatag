import { Initialisable } from '../services/types'
import { PlutusData } from '@blaze-cardano/core'

import { MintRedeemer, StateHolderStateHolder } from '@adatag/common/plutus'

export interface TreeWorkerService extends Initialisable {
  checkIfAdatagMinted(adatag: string): Promise<boolean>
  checkIfAdatagNotMinted(adatag: string): Promise<boolean>
  createMintingDetails(adatag: string): Promise<{ datum: StateHolderStateHolder['oldState']; redeemer: MintRedeemer }>

  getDepositDetails(adatag: string): Promise<
    | {
        txId: string
        outputIndex: number
        beneficiary: string
        deadline: number
      }
    | undefined
  >
}
