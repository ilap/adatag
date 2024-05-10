import {
  UTxO,
  Tx,
  TxComplete,
  Translucent,
  toUnit,
  fromText,
  Data,
  AddressDetails,
} from 'translucent-cardano'
import { MintingService } from './types.ts'
import { calculateDeposit, stringifyData } from '../utils.ts'
import { MAXDEPOSITLENGTH, MINDEPOSIT, MS } from '../configs/settings.ts'
import Config from '../configs/genesis-config.json'

import * as T from '../configs/types.ts'

export class AdatagMintingService implements MintingService {
  constructor(private translucent: Translucent) { }

  static getMinDeposit(adatag: string): bigint {
    return calculateDeposit(
      adatag,
      Config.adatagMinting.params.depositBase,
      MINDEPOSIT,
      MAXDEPOSITLENGTH,
    )
  }


  async getAssetUTxo(
    pid: string,
    assetName: string,
  ): Promise<UTxO | undefined> {
    const unit = toUnit(pid, fromText(assetName))
    return await this.translucent!.utxoByUnit(unit)
  }


  async buildBaseTx(
    adatag: string,
    useAdaHandle: boolean,
    userAddress: string | undefined,
    deposit: bigint,
  ): Promise<Tx> {

    // Set the default values for the transaction
    const adatagHex = fromText(adatag)


    // Create a new transaction
    // 1. .newTx()
    let tx = this.translucent!.newTx()

    // Set the minting- and state-hodler policies' reference UTxOs
    // TODO: Find some better method to fetch these static references from chain.
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

    // Check whether the time lock deposit is still active or not.
    const now = Date.now()
    // It's in milliseconds due to the PLC.
    const timelockActive = Config.adatagMinting.params.deactivationTime.epoch > now * MS

    console.warn(`$$$$$$$$$$$$$$$$$$$$ TL ACTIVE: ${timelockActive} .... Using handle ${useAdaHandle}`)

    // When time lock deposits is active
    if (timelockActive || true) {
     
      // If user has the same ada handle as the minting adatag and decided to use it for avoiding
      // time lock teposits.
      if (useAdaHandle) {
        console.warn(`##### USING ADAHANDLE`)
        const handleAsset = { [Config.adatagMinting.params.adahandle + adatagHex]: 1n }
        tx = tx.payToAddress(userAddress!, handleAsset)
      } else {
        // Times are in milliseconds in Plutus Core
        const deadLine = BigInt(
          (now + Config.adatagMinting.params.lockingDays.days * 86400 + 3600) * MS,
        )

        // Beneficiary is always the user!
        const { paymentCredential } = this.translucent.utils.getAddressDetails(userAddress!) as AddressDetails


        // Create the Timelock deposit datum
        const datum: T.TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential!.hash as unknown as string,
          deadLine: deadLine,
        }

        const datumCbor = Data.to(datum, T.TimeDepositDatum.datum)

        // Retrieve the TielockDeposit's UTxO
        const timelockRefUtxo = await this.translucent!.utxosByOutRef([
          {
            txHash: Config.genesisTransaction,
            outputIndex: Config.timelockScript.refIndex,
          },
        ])
        refUtxos.concat(timelockRefUtxo)

        tx = tx.payToContract(
          Config.timelockScript.scriptAddress,
          { inline: datumCbor },
          { lovelace: deposit },
        )
      }
    }

    // 3. .readFrom(mpRefUtxo.concat(shRefUtxo))
    return tx.readFrom(refUtxos)
  }

  async finaliseTx(
    tx: Tx,
    adatag: string,
    userAddress: string | undefined,
    datum: string,
    redeemer: string,
  ): Promise<Tx> {
    const mintAmount = 1n

    const authUnit = Config.authTokenScript.policyId + fromText(adatag[0])
    const authAsset = { [authUnit]: mintAmount }
    const authUtxo = await this.translucent!.utxoByUnit(authUnit)
    console.log(`##### AUTHUTXO: ${stringifyData(authUtxo)}`)

    const mintValue = {
      [Config.adatagMinting.policyId + fromText(adatag)]: mintAmount,
    }
    const now: number = Date.now()

    // 5. .payToContract( bd.stateholderScript.scriptAddress, { inline: state }, authToken,)
    const finalisedTx = tx
      .payToContract(
        Config.stateholderScript.scriptAddress,
        { inline: datum },
        authAsset,
      )
      // 2. .collectFrom([authUtxo], Data.void())
      .collectFrom([authUtxo], Data.void())
      // 6. .mintAssets(mintValue, rdmr)
      .mintAssets(mintValue, redeemer)
      // FIXME: user address...
      .payToAddress(userAddress!, mintValue)
      // It is always required for deactivation's time checking.
      // 7. .validFrom(time)
      .validFrom(now)


    return finalisedTx
  }
}
