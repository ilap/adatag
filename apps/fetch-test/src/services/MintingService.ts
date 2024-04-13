import { UTxO, Tx, TxComplete, Translucent, toUnit, fromText, Data } from 'translucent-cardano'
import { MintingService } from './types.ts'
import { calculateDeposit, stringifyData } from '../utils.ts'
import { MAXDEPOSITLENGTH, MINDEPOSIT } from '../configs/settings.ts'
import Config from '../configs/genesis-config.json'

import * as T from '../configs/types.ts'

export class AdatagMintingService implements MintingService {
  constructor(private translucent: Translucent) {}

  async getAssetUTxo(pid: string, assetName: string): Promise<UTxO | undefined> {
    const unit = toUnit(pid, fromText(assetName))
    return await this.translucent!.utxoByUnit(unit)
  }

  async isAdatagOnChain(adatag: string): Promise<boolean> {
    const result = await this.getAssetUTxo(Config.adatagMinting.policyId, adatag)
    return result !== undefined
  }

  async getHandleUtxo(adatag: string): Promise<UTxO | undefined> {
    return await this.getAssetUTxo(Config.adatagMinting.params.adahandle, adatag)
  }

  getMinDeposit(adatag: string): bigint {
    return calculateDeposit(
      adatag,
      Config.adatagMinting.params.depositBase,
      MINDEPOSIT,
      MAXDEPOSITLENGTH
    )
  }

  async buildBaseTx(adatag: string, handleUtxo: UTxO | undefined, deposit: bigint): Promise<Tx> {
    // Set the default values for the transaction
    const adatagHex = fromText(adatag)
    const mintValue = { [Config.adatagMinting.policyId + adatagHex]: 1n }

    // Create a new transaction
    // 1. .newTx()
    let tx = this.translucent!.newTx()

    // Set the minting- and state-hodler policies' reference UTxOs
    const refUtxos = await this.translucent!.utxosByOutRef([
      {
        txHash: Config.genesisTransaction,
        outputIndex: Config.stateholderScript.refIndex,
      },
      {
        txHash: Config.genesisTransaction,
        outputIndex: Config.adatagMinting.refIndex,
      },
    ])

    const now = Date.now()
    const timelockActive = Config.adatagMinting.params.deactivationTime.epoch > now * 1000 // in ms

    //
    if (timelockActive /* TODO: || true */) {
      // FIXME: it must be set in the calling function...

      if (handleUtxo) {
        // TODO: Test this scenario
        mintValue[Config.adatagMinting.params.adahandle + adatagHex] = 1n
        tx.collectFrom([handleUtxo])
      } else {
        // Times are in milliseconds in Plutus Core
        const deadLine = BigInt(
          (now + Config.adatagMinting.params.lockingDays.days * 86400 + 3600) * 1000
        )

        const { paymentCredential } = this.translucent.utils.getAddressDetails(
          await this.translucent.wallet.address()
        )
        // Create the TL datum
        const datum: T.TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential as unknown as string,
          deadLine: deadLine,
        }

        const timelockDatum = Data.to(datum, T.TimeDepositDatum.datum)

        // Retrieve the TielockDeposit's UTxO
        const timelockRefUtxo = await this.translucent!.utxosByOutRef([
          {
            txHash: Config.genesisTransaction,
            outputIndex: Config.timelockScript.refIndex,
          },
        ])
        refUtxos.concat(timelockRefUtxo)

        // FIXME: maxLength and minDeposit should come from fonfig...
        const deposit = calculateDeposit(adatag, Config.adatagMinting.params.depositBase, 5, 5)

        tx = tx.payToContract(
          Config.timelockScript.scriptAddress,
          { inline: timelockDatum },
          { lovelace: deposit }
        )
      }
    }

    //debugMessage(`####1 ${await rest.toString()}`)
    // 3. .readFrom(mpRefUtxo.concat(shRefUtxo))
    return tx.readFrom(refUtxos)
  }

  async finaliseTx(tx: Tx, adatag: string, datum: string, redeemer: string): Promise<Tx> {
    const mintAmount = 1n

    const authUnit = Config.authTokenScript.policyId + fromText(adatag[0])
    const authAsset = { [authUnit]: mintAmount }
    const authUtxo = await this.translucent!.utxoByUnit(authUnit)
    console.log(`##### AUTHUTXO: ${stringifyData(authUtxo)}`)

    const mintValue = { [Config.adatagMinting.policyId + fromText(adatag)]: mintAmount }
    const now: number = Date.now()

    // 5. .payToContract( bd.stateholderScript.scriptAddress, { inline: state }, authToken,)
    const finalisedTx = tx
      .payToContract(Config.stateholderScript.scriptAddress, { inline: datum }, authAsset)
      // 2. .collectFrom([authUtxo], Data.void())
      .collectFrom([authUtxo], Data.void())
      // 6. .mintAssets(mintValue, rdmr)
      .mintAssets(mintValue, redeemer)
      .payToAddress(Config.timelockScript.params.collectorAddr, mintValue)
      // It is always required for deactivation's time checking.
      // 7. .validFrom(time)
      .validFrom(now)
    // 4. .payToAddress(userAddress, mintValue)

    return finalisedTx
  }

  completeTx(finalisedTx: Tx): Promise<TxComplete> {
    throw new Error('Method not implemented.')
  }
  submitTx(completedTx: TxComplete): Promise<string> {
    throw new Error('Method not implemented.')
  }
  waitforConfirmation(txHash: string, timeout: number): Promise<boolean> {
    throw new Error('Method not implemented.')
  }
}
