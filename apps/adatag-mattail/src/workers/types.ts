import { Initialisable } from '../services/types.ts'

export interface TreeWorkerService extends Initialisable {
  checkIfAdatagMinted(adatag: string): Promise<boolean>
  createMintingDetails(adatag: string): Promise<{
    datum: string
    redeemer: string
  }>
}
