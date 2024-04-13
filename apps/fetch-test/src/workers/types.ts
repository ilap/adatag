import { Initialisable } from '../services/types.ts'

export interface TreeWorkerService extends Initialisable {
  createMintingDetails(adatag: string): Promise<{
    datum: string
    redeemer: string
  }>
}
