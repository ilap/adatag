import { Tx, Blaze, Data, UTxO } from '@blaze-cardano/sdk'
import { ClaimingService } from './types'
import { TimeDepositTimedeposit } from '@adatag/common/plutus'

import { stringifyData } from '../utils'
import { genesisConfig } from '../utils/config'

export class AdatagClaimingService implements ClaimingService {
  constructor(private blaze: Blaze) {}

  /**
   * When collecting the
   *
   * @param action
   * @param beneficiary The users or the collector
   * @param deposit
   * @param donation
   * @returns
   */
  async buildClaimTx(
    action: 'Collect' | 'Redeem',
    beneficiary: string,
    donation: bigint,
    depositUtxos: UTxO[]
  ): Promise<Tx> {
    let tx = this.blaze!.newTransaction().addSignerKey(beneficiary)

    const timelockRefUtxo = await this.blaze!.utxosByOutRef([
      {
        txHash: genesisConfig!.genesisTransaction,
        outputIndex: genesisConfig!.timelockScript.refIndex,
      },
    ])

    console.log(`DONATION  : ${donation} .... ${action}`)
    console.log(` DEP UTXOS: ${stringifyData(depositUtxos)}`)
    console.log(` TLO UTXOS: ${stringifyData(timelockRefUtxo)}`)

    if (action === 'Redeem' && donation > 0n) {
      tx = tx.payToAddress(genesisConfig!.timelockScript.params.collectorAddr, {
        lovelace: donation * 1_000_000n,
      })
    }

    // Create the redeemer
    const rdmr = this.createRedeemer(action)

    // A valid TTL must exist.
    const validFrom = Math.floor(Date.now() / 1000) * 1000

    return tx.addSignerKey(beneficiary).readFrom(timelockRefUtxo).collectFrom(depositUtxos, rdmr).validFrom(validFrom)
  }

  private createRedeemer(action: 'Collect' | 'Redeem'): string {
    // Redeem
    const claimRedeemer: typeof TimeDepositTimedeposit.rdmr = action
    const redeemer = Data.to(claimRedeemer, TimeDepositTimedeposit.rdmr)
    return redeemer
  }
}
