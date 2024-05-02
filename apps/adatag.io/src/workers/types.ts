import { Initialisable } from '../services/types'

export interface TreeWorkerService extends Initialisable {
  checkIfAdatagMinted(adatag: string): Promise<boolean>
  checkIfAdatagNotMinted(adatag: string): Promise<boolean>
  createMintingDetails(adatag: string): Promise<{ datum: string; redeemer: string }>
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
