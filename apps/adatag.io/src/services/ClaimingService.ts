import { Blaze, Data, Provider, Wallet, TxBuilder } from '@blaze-cardano/sdk'
import { ClaimingService } from './types'
import { TimeDepositTimedeposit } from '@adatag/common/plutus'

import { stringifyData } from '../utils'
import { genesisConfig } from '../utils/config'
import {
  Address,
  Ed25519KeyHashHex,
  PlutusData,
  Transaction,
  TransactionId,
  TransactionInput,
  TransactionUnspentOutput,
} from '@blaze-cardano/core'
import { fromText, unixTimeToSlot } from '@adatag/common/utils'

export class AdatagClaimingService implements ClaimingService {
  constructor(private blaze: Blaze<Provider, Wallet>) {}

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
    depositUtxos: TransactionUnspentOutput[]
  ): Promise<TxBuilder> {
    const signerHash = Ed25519KeyHashHex(beneficiary)

    let tx = this.blaze!.newTransaction()

    //const coll = tx.
    const timelockRefUtxo = await this.blaze!.provider.resolveUnspentOutputs([
      new TransactionInput(
        TransactionId(genesisConfig!.genesisTransaction),
        BigInt(genesisConfig!.timelockScript.refIndex)
      ),
    ])

    console.log(`DONATION  : ${donation} .... ${action}`)
    // DEBUG: console.log(` DEP UTXOS: ${stringifyData(depositUtxos.map(utxo => utxo.toCore()))}`)
    // DEBUG: console.log(` TLO UTXOS: ${stringifyData(timelockRefUtxo.map(utxo => utxo.toCore()))}`)

    if (action === 'Redeem' && donation > 0n) {
      tx = tx.payLovelace(Address.fromBech32(genesisConfig!.timelockScript.params.collectorAddr), donation * 1_000_000n)
    }

    // Create the redeemer
    const rdmr = this.createRedeemer(action)

    // A valid TTL must exist. It's set to now - 10 minutes
    const validFrom = (Math.floor(Date.now() / 1000) - 600) * 1000

    const finalBuilder = depositUtxos
      .reduce((tx, depositUtxo) => tx.addInput(depositUtxo, rdmr), tx)
      // It is a reference UtXO
      .addReferenceInput(timelockRefUtxo[0])
      .addRequiredSigner(signerHash)
      .setValidFrom(unixTimeToSlot(validFrom, genesisConfig!.network))

    return finalBuilder
  }

  private createRedeemer(action: 'Collect' | 'Redeem'): PlutusData {
    // Redeem
    const claimRedeemer: typeof TimeDepositTimedeposit.rdmr = action
    const redeemer = Data.to(claimRedeemer, TimeDepositTimedeposit.rdmr)
    return redeemer
  }
}
