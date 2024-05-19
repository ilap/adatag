import { UTxO, Tx, Translucent, toUnit, fromText, Data, AddressDetails } from 'translucent-cardano'
import { MintingService } from './types'
import { calculateDeposit, stringifyData } from '../utils'
import { MAXDEPOSITLENGTH, MINDEPOSIT } from '../configs/settings'
import { genesisConfig } from '../utils/config'
import { TimeDepositDatum } from '@adatag/common/plutus'

/**
 * Implementation of the MintingService interface for Adatag minting.
 */
export class AdatagMintingService implements MintingService {
  /**
   * The Translucent instance used for building and finalizing transactions.
   */
  constructor(private translucent: Translucent) {}

  /**
   * Calculates the minimum deposit required for minting an Adatag.
   * @param adatag The Adatag to calculate the deposit for.
   * @returns The minimum deposit required in Lovelace.
   */
  static getMinDeposit(adatag: string): bigint {
    return calculateDeposit(adatag, genesisConfig!.adatagMinting.params.depositBase, MINDEPOSIT, MAXDEPOSITLENGTH)
  }

  /**
   * Retrieves the UTxO containing the specified asset.
   * @param pid The policy ID of the asset.
   * @param assetName The name of the asset.
   * @returns The UTxO containing the specified asset, or undefined if not found.
   */
  async getAssetUTxo(pid: string, assetName: string): Promise<UTxO | undefined> {
    const unit = toUnit(pid, fromText(assetName))
    return await this.translucent!.utxoByUnit(unit)
  }

  /**
   * Builds a base transaction for minting an Adatag.
   * @param adatag The Adatag to mint.
   * @param useAdaHandle Whether to use the user's Ada handle for avoiding time lock deposits.
   * @param userAddress The address of the user minting the Adatag.
   * @param deposit The deposit amount in Lovelace.
   * @returns The base transaction ready for signing.
   */
  async buildBaseTx(
    adatag: string,
    useAdaHandle: boolean,
    userAddress: string | undefined,
    deposit: bigint
  ): Promise<Tx> {
    // Set the default values for the transaction
    const adatagHex = fromText(adatag)

    // 1. Create a new transaction
    let tx = this.translucent!.newTx()

    // Set the minting- and state-holder policies' reference UTxOs
    // TODO: Find some better method to fetch these static references from chain.
    const refUtxos = await this.translucent!.utxosByOutRef([
      {
        txHash: genesisConfig!.genesisTransaction,
        outputIndex: genesisConfig!.stateholderScript.refIndex,
      },
      {
        txHash: genesisConfig!.genesisTransaction,
        outputIndex: genesisConfig!.adatagMinting.refIndex,
      },
    ])

    // required for deactivation checking
    const validFrom = Math.floor(Date.now() / 1000) * 1000

    // required for deadline check
    // TODO: use proper time buffer than the static 60secs
    const validTo = validFrom + 60000

    // TODO:
    // IDEA: We should remove tje deactivation time, meaning it should always active.
    const timelockActive = genesisConfig!.adatagMinting.params.deactivationTime.epoch > validFrom

    console.warn(
      `$$$$$$$$$$$$$$$$$$$$ TL ACTIVE: ${validFrom} ${
        genesisConfig!.adatagMinting.params.deactivationTime.epoch
      } ${timelockActive} .... Using handle ${useAdaHandle}`
    )

    // When time lock deposits is active
    if (timelockActive) {
      // If user has the same ada handle as the minting adatag and decided to use it for avoiding
      // time lock teposits.
      if (useAdaHandle) {
        console.warn(`##### USING ADAHANDLE`)
        const handleAsset = {
          [genesisConfig!.adatagMinting.params.adahandle + adatagHex]: 1n,
        }
        tx = tx.payToAddress(userAddress!, handleAsset)
      } else {
        // Times are in milliseconds in Plutus Core
        // IMPORTANT: to + bd.adatagMinting.params.lockingDays.ms < deadLine
        // console.warn(`Valid deadline: ${to + bd.adatagMinting.params.lockingDays.ms < deadLine}`);

        // TODO: use proper time buffer instead of 10secs
        const deadLine = BigInt(validTo + genesisConfig!.adatagMinting.params.lockingDays.ms + 10000)

        console.log(`### DEADLINE ${deadLine}`)

        // Beneficiary is always the user!
        const { paymentCredential } = this.translucent.utils.getAddressDetails(userAddress!) as AddressDetails

        // Create the Timelock deposit datum
        const datum: TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential!.hash as unknown as string,
          deadLine: deadLine,
        }

        console.warn(`# TL: DATUM: ${stringifyData(datum)}`)
        const datumCbor = Data.to(datum, TimeDepositDatum.datum)

        // Retrieve the TielockDeposit's UTxO
        const timelockRefUtxo = await this.translucent!.utxosByOutRef([
          {
            txHash: genesisConfig!.genesisTransaction,
            outputIndex: genesisConfig!.timelockScript.refIndex,
          },
        ])
        refUtxos.concat(timelockRefUtxo)

        console.log(`#### DEPOSIT PAID TO CONTRACT ${deposit * 1_000_000n}`)
        tx = tx
          .payToContract(
            genesisConfig!.timelockScript.scriptAddress,
            { inline: datumCbor },
            { lovelace: deposit * 1_000_000n }
          )
          .validTo(validTo)
      }
    }

    // 3. .readFrom(mpRefUtxo.concat(shRefUtxo))
    return tx.readFrom(refUtxos).validFrom(validFrom)
  }

  /**
   * Finalizes the transaction by minting the Adatag and paying the user.
   * @param tx The base transaction to finalize.
   * @param adatag The Adatag to mint.
   * @param userAddress The address of the user minting the Adatag.
   * @param datum The datum to be stored in the minted Adatag.
   * @param redeemer The redeemer to be used for minting the Adatag.
   * @returns The finalized transaction ready for submission.
   */
  async finaliseTx(
    tx: Tx,
    adatag: string,
    userAddress: string | undefined,
    datum: string,
    redeemer: string
  ): Promise<Tx> {
    const mintAmount = 1n

    const authUnit = genesisConfig!.authTokenScript.policyId + fromText(adatag[0])
    const authAsset = { [authUnit]: mintAmount }
    const authUtxo = await this.translucent!.utxoByUnit(authUnit)
    console.log(`##### AUTHUTXO: ${stringifyData(authUtxo)}`)

    const mintValue = {
      [genesisConfig!.adatagMinting.policyId + fromText(adatag)]: mintAmount,
    }

    // 5. .payToContract( bd.stateholderScript.scriptAddress, { inline: state }, authToken,)
    const finalisedTx = tx
      .payToContract(genesisConfig!.stateholderScript.scriptAddress, { inline: datum }, authAsset)
      // 2. .collectFrom([authUtxo], Data.void())
      .collectFrom([authUtxo], Data.void())
      // 6. .mintAssets(mintValue, rdmr)
      .mintAssets(mintValue, redeemer)
      // FIXME: user address...
      .payToAddress(userAddress!, mintValue)

    return finalisedTx
  }
}
