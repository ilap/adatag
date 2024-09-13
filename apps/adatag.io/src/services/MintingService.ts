import { Blaze, Data, Wallet, Provider, TxBuilder } from '@blaze-cardano/sdk'
import { MintingService } from './types'
import { calculateDeposit, stringifyData } from '../utils'
import { MAXDEPOSITLENGTH, MINDEPOSIT } from '../configs/settings'
import { genesisConfig } from '../utils/config'
import { TimeDepositDatum } from '@adatag/common/plutus'
import { fromText, unixTimeToSlot } from '@adatag/common/utils'

import {
  AssetId,
  Address,
  TransactionUnspentOutput,
  TokenMap,
  TransactionInput,
  TransactionId,
  Value,
  PlutusData,
  Datum,
  PolicyId,
  HexBlob,
} from '@blaze-cardano/core'

/**
 * Implementation of the MintingService interface for Adatag minting.
 */
export class AdatagMintingService implements MintingService {
  /**
   * The Blaze instance used for building and finalizing transactions.
   */
  constructor(private blaze: Blaze<Provider, Wallet>) {}

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
  async getAssetUTxo(pid: string, assetName: string): Promise<TransactionUnspentOutput | undefined> {
    const assetId = AssetId(pid + fromText(assetName))
    return await this.blaze!.provider.getUnspentOutputByNFT(assetId)
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
  ): Promise<TxBuilder> {
    // Set the default values for the transaction
    const adatagHex = fromText(adatag)

    // 1. Create a new transaction
    let tx = this.blaze!.newTransaction()

    // Set the minting- and state-holder policies' reference UTxOs
    // TODO: Find some better method to fetch these static references from chain.
    const refInputs = [
      new TransactionInput(
        TransactionId(genesisConfig!.genesisTransaction),
        BigInt(genesisConfig!.stateholderScript.refIndex)
      ),
      new TransactionInput(
        TransactionId(genesisConfig!.genesisTransaction),
        BigInt(genesisConfig!.adatagMinting.refIndex)
      ),
    ]

    // required for deactivation checking
    const currTime = Math.floor(Date.now() / 1000) * 1000

    // lower bound of validFrom is 5 mins before the current time.
    // TODO: Find proper value for the lower and upper bounds, then the current [5 mins before, 1hr after the current time]
    const validFrom = currTime - 5 * 60 * 1000
    const validTo = validFrom + 60 * 60 * 1000

    // TODO:
    // IDEA: We should remove tje deactivation time, meaning it should always active.
    const timelockActive = genesisConfig!.adatagMinting.params.deactivationTime.epoch > validFrom

    console.warn(
      `$$$$$$$$$$$$$$$$$$$$ TL ACTIVE: ${validFrom} ${
        genesisConfig!.adatagMinting.params.deactivationTime.epoch
      } ${timelockActive} .... Using handle ${useAdaHandle}`
    )

    const address = Address.fromBech32(userAddress!)
    // When time lock deposits is active
    if (timelockActive) {
      // If user has the same ada handle as the minting adatag and decided to use it for avoiding
      // time lock teposits.
      if (useAdaHandle) {
        console.warn(`##### USING ADAHANDLE`)

        const handleAssetId = AssetId(genesisConfig!.adatagMinting.policyId + fromText(adatag))

        const handleToken: TokenMap = new Map<AssetId, bigint>([[handleAssetId, 1n]])

        tx = tx.payAssets(address, new Value(0n, handleToken))
      } else {
        // Times are in milliseconds in Plutus Core
        // IMPORTANT: to + bd.adatagMinting.params.lockingDays.ms < deadLine
        // console.warn(`Valid deadline: ${to + bd.adatagMinting.params.lockingDays.ms < deadLine}`);

        // TODO: use proper time buffer instead of 10secs
        const deadLine = BigInt(validTo + genesisConfig!.adatagMinting.params.lockingDays.ms + 10000)

        console.warn(`### DEADLINE ${deadLine}`)
        // Beneficiary is always the user!
        const paymentCredential = address.asBase()!.getPaymentCredential()
        //const { paymentCredential } = this.blaze.utils.getAddressDetails(userAddress!) as AddressDetails

        // Create the Timelock deposit datum
        const datum: TimeDepositDatum['datum'] = {
          beneficiary: paymentCredential!.hash as unknown as string,
          deadLine: deadLine,
        }

        const timelockRefInput = new TransactionInput(
          TransactionId(genesisConfig!.genesisTransaction),
          BigInt(genesisConfig!.timelockScript.refIndex)
        )

        // Retrieve the TielockDeposit's UTxO

        refInputs.push(timelockRefInput)

        console.warn(`#### DEPOSIT PAID TO CONTRACT ${deposit * 1_000_000n}`)

        console.warn(`# TL: DATUM: ${stringifyData(datum)}`)
        const datumData = Data.to(datum, TimeDepositDatum.datum)

        const slot = unixTimeToSlot(validTo, genesisConfig!.network)
        console.warn(`SLOT: ${slot} ... ${genesisConfig!.network}`)

        tx = tx
          .lockLovelace(
            Address.fromBech32(genesisConfig!.timelockScript.scriptAddress),
            deposit * 1_000_000n,
            datumData
          )
          .setValidUntil(slot)
      }
    }

    const refUtxos = await this.blaze!.provider.resolveUnspentOutputs(refInputs)
    // 3. .readFrom(mpRefUtxo.concat(shRefUtxo))
    tx = tx.addReferenceInput(refUtxos[0]).addReferenceInput(refUtxos[1])

    if (refUtxos.length > 2) {
      tx = tx.addReferenceInput(refUtxos[2])
    }
    return tx.setValidFrom(unixTimeToSlot(validFrom, genesisConfig!.network))
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
    builder: TxBuilder,
    adatag: string,
    userAddress: string | undefined,
    datum: Datum,
    redeemer: PlutusData
  ): Promise<TxBuilder> {
    const mintAmount = 1n

    /// addMint
    const mintPolid = PolicyId(genesisConfig!.adatagMinting.policyId)
    const mintAssetId = AssetId(genesisConfig!.adatagMinting.policyId + fromText(adatag))

    /// lockAssets
    const authAssetId = AssetId(genesisConfig!.authTokenScript.policyId + fromText(adatag[0]))
    const authToken: TokenMap = new Map<AssetId, bigint>([[authAssetId, mintAmount]])
    const authAddress = Address.fromBech32(genesisConfig!.stateholderScript.scriptAddress)

    /// payAssets
    const address = Address.fromBech32(userAddress!)
    const mintToken: TokenMap = new Map<AssetId, bigint>([[mintAssetId, mintAmount]])

    /// addInput
    const authUtxo = await this.blaze!.provider.getUnspentOutputByNFT(authAssetId)

    const finalisedTx = builder
      .addMint(mintPolid, new Map([[AssetId.getAssetName(mintAssetId), 1n]]), redeemer)
      .lockAssets(authAddress, new Value(0n, authToken), datum)
      .payAssets(address, new Value(0n, mintToken))
      .addInput(authUtxo, Data.void())

    return finalisedTx
  }
}
