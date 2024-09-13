//import { Script, Data, Assets, Blaze, UTxO, fromText, C } from 'blaze-cardano'

import { Blaze, Data, Wallet, Provider, cborToScript } from '@blaze-cardano/sdk'
import {
  Script,
  AssetName,
  TokenMap,
  Value,
  AssetId,
  PolicyId,
  Address,
  TransactionUnspentOutput,
  addressFromValidator,
} from '@blaze-cardano/core'

import * as P from '@adatag/common/plutus'
import { fromText, stringifyData } from '@adatag/common/utils'
import { emptyHash, combineThreeHashes, hashVal } from '@adatag/integri-tree'
import { GenesisConfig, GenesisParams } from '@adatag/common/config'

export class Bootstrap {
  // Utility function to calculate days
  public static days = (n: string): number => Math.floor(eval(n) * 86400000)

  // Placeholder details for the Bootstrap class
  private static defaultDetails: GenesisConfig = {
    network: '',
    hashAlg: '',
    genesisTransaction: '',
    bootstrapTime: {
      epoch: 0,
      date: '',
    },
    referenceScript: {
      scriptHash: '',
      scriptAddress: '',
    },
    authTokenScript: {
      policyId: '',
      params: {
        genesis_utxo: {
          txHash: '',
          outputIndex: 0,
        },
      },
    },
    timelockScript: {
      scriptHash: '',
      scriptAddress: '',
      params: {
        collectorAddr: '',
        collectionTime: {
          epoch: 0,
          date: '',
        },
      },
      refIndex: 0,
    },
    stateholderScript: {
      scriptHash: '',
      scriptAddress: '',
      params: {
        authToken: '',
      },
      refIndex: 0,
    },
    adatagMinting: {
      policyId: '',
      params: {
        lockingDays: {
          days: 0,
          ms: 0,
        },
        deactivationTime: {
          epoch: 0,
          date: '',
        },
        depositBase: 0,
        adahandle: '',
      },
      refIndex: 0,
    },
  }

  // Deploys the contract, saves the result to a file, and returns the details
  public static async deployAndSave(
    file: string,
    blaze: Blaze<Provider, Wallet>,
    deployerUtxo: TransactionUnspentOutput,
    params: GenesisParams
  ): Promise<GenesisConfig> {
    const result = await Bootstrap.deploy(blaze, deployerUtxo, params)
    Bun.write(Bun.file(file), JSON.stringify(result, null, '  '))
    return result
  }

  public static async deploy(
    blaze: Blaze<Provider, Wallet>,
    deployerUtxo: TransactionUnspentOutput,
    params: GenesisParams
  ): Promise<GenesisConfig> {
    const epoch = Date.now()
    const defaultDetails: GenesisConfig = {
      ...this.defaultDetails,
      network: params.networkName,
      hashAlg: params.hashAlg,
      bootstrapTime: {
        epoch: epoch,
        date: new Date(epoch).toString(),
      },
    }

    const refDetails = this.getReferenceDetails(blaze, params, defaultDetails)
    const { authMintingPolicy, authDetails } = this.getAuthTokenScript(blaze, deployerUtxo, refDetails)
    const { timelockScript, timelockDetails } = this.getTimelockDetails(blaze, params, authDetails)
    const { mintingPolicy, stateholderValidator, mintingDetails } = this.getMintingDetails(params, timelockDetails)

    return await this.buildTx(blaze, deployerUtxo, mintingDetails, {
      authMintingPolicy,
      timelockScript,
      stateholderValidator,
      mintingPolicy,
    })
  }

  private static getReferenceDetails(blaze: Blaze<Provider, Wallet>, params: GenesisParams, details: GenesisConfig) {
    const referenceScriptCbor = new P.AlwaysFailAlwaysFail().script

    const referenceScript = cborToScript(referenceScriptCbor, 'PlutusV2')
    const alwaysFailHash = referenceScript.hash()
    const referenceAddress = addressFromValidator(params.networkName === 'Mainnet' ? 1 : 0, referenceScript)

    return {
      ...details,
      referenceScript: {
        scriptAddress: referenceAddress.toBech32().toString(),
        scriptHash: alwaysFailHash.toString(),
      },
    }
  }

  private static getAuthTokenScript(
    blaze: Blaze<Provider, Wallet>,
    deployerUtxo: TransactionUnspentOutput,
    details: GenesisConfig
  ) {
    const txHash = deployerUtxo.input().transactionId().toString()
    const outputIndex = deployerUtxo.input().index()
    const authMintingPolicyCbor = new P.OneshotAuthToken({
      transactionId: { hash: txHash },
      outputIndex: outputIndex,
    }).script

    const authMintingPolicy = cborToScript(authMintingPolicyCbor, 'PlutusV2')

    const policyId = PolicyId(authMintingPolicy.hash())

    return {
      authMintingPolicy,
      authDetails: {
        ...details,
        authTokenScript: {
          policyId: policyId.toString(),
          params: {
            genesis_utxo: {
              txHash: txHash,
              outputIndex: Number(outputIndex),
            },
          },
        },
      },
    }
  }

  private static getTimelockDetails(blaze: Blaze<Provider, Wallet>, params: GenesisParams, details: GenesisConfig) {
    const collectionTime = details.bootstrapTime.epoch + this.days(params.collectionTime.toString())

    const addr = Address.fromBech32(params.collectorAddress)
    const cred = addr.asBase()?.getPaymentCredential().hash

    const timelockCbor = new P.TimeDepositTimedeposit({
      collector: cred!,
      collectionTime: BigInt(collectionTime),
    }).script

    const timelockScript = cborToScript(timelockCbor, 'PlutusV2')
    const timelockHash = timelockScript.hash()
    const timelockAddress = addressFromValidator(params.networkName === 'Mainnet' ? 1 : 0, timelockScript)

    return {
      timelockScript: timelockScript,
      timelockDetails: {
        ...details,
        timelockScript: {
          scriptHash: timelockHash.toString(),
          scriptAddress: timelockAddress.toBech32().toString(),
          params: {
            collectorAddr: params.collectorAddress,
            collectionTime: {
              epoch: collectionTime,
              date: new Date(collectionTime).toString(),
            },
          },
          refIndex: -1,
        },
      },
    }
  }

  private static getMintingDetails(params: GenesisParams, details: GenesisConfig) {
    const authPolicyId = details.authTokenScript.policyId
    const stateholderValidatorCbor = new P.StateHolderStateHolder(authPolicyId).script

    const stateholderValidator = cborToScript(stateholderValidatorCbor, 'PlutusV2')
    const stateholderHash = stateholderValidator.hash()
    const stateholderAddress = addressFromValidator(params.networkName === 'Mainnet' ? 1 : 0, stateholderValidator)

    const deactivationTime = details.bootstrapTime.epoch + this.days(params.deactivationTime.toString())
    const lockingDays = this.days(params.lockingDays.toString())

    const scriptPparams = {
      authToken: authPolicyId,
      stateHolder: stateholderHash.toString(),
      timedepositValidator: details.timelockScript.scriptHash,
      deactivationTime: BigInt(deactivationTime),
      depositBase: BigInt(params.depositBase),
      lockingDays: BigInt(lockingDays),
      adahandle: params.adahandle,
    }

    //console.log(`scriptPparams1: ${stringifyData(params)}`)
    //console.log(`scriptPparams2: ${stringifyData(scriptPparams)}`)
    const mintingPolicyCbor = new P.AdatagAdatagMinting(scriptPparams).script
    const mintingPolicy = cborToScript(mintingPolicyCbor, 'PlutusV2')

    //console.log(`mintingPolicy: ${mintingPolicy.hash().toString()}`)

    return {
      mintingPolicy: mintingPolicy,
      stateholderValidator,
      mintingDetails: {
        ...details,
        stateholderScript: {
          scriptHash: stateholderHash.toString(),
          scriptAddress: stateholderAddress.toBech32().toString(),
          params: {
            authToken: details.authTokenScript.policyId,
          },
          refIndex: -1,
        },
        adatagMinting: {
          policyId: mintingPolicy.hash().toString(),
          params: {
            lockingDays: {
              days: params.lockingDays,
              ms: lockingDays,
            },
            deactivationTime: {
              epoch: deactivationTime,
              date: new Date(deactivationTime).toString(),
            },
            depositBase: params.depositBase,
            adahandle: params.adahandle,
          },
          refIndex: -1,
        },
      },
    }
  }

  private static async buildTx(
    blaze: Blaze<Provider, Wallet>,
    deployerUtxO: TransactionUnspentOutput,
    details: GenesisConfig,
    scripts: {
      authMintingPolicy: Script
      timelockScript: Script
      stateholderValidator: Script
      mintingPolicy: Script
    }
  ): Promise<GenesisConfig> {
    const refAddr = details.referenceScript.scriptAddress
    const tx = blaze
      .newTransaction()
      .addUnspentOutputs([deployerUtxO])
      .lockLovelace(Address.fromBech32(refAddr), 0n, Data.void(), scripts.stateholderValidator)
      .lockLovelace(Address.fromBech32(refAddr), 0n, Data.void(), scripts.mintingPolicy)
      .lockLovelace(Address.fromBech32(refAddr), 0n, Data.void(), scripts.timelockScript)

    const sateholderAddress = details.stateholderScript.scriptAddress

    const policyId = PolicyId(details.authTokenScript.policyId)
    const assets: Map<AssetName, bigint> = new Map()

    for (let i = 97; i <= 122; i++) {
      // const val = [48, i - 1, i + 1]
      // FIXME: The hash alg should be defined only in integri-tree module
      const hash_alg = details.hashAlg

      const val: P.Val = {
        xi: String.fromCharCode(48),
        xa: String.fromCharCode(i - 1),
        xb: String.fromCharCode(i + 1),
      }

      const hv = hashVal(val)
      const root_hash = combineThreeHashes(hv, emptyHash, emptyHash)

      const state: P.StateHolderStateHolder['oldState'] = {
        operationCount: 0n,
        operation: 'AdatagRemoved',
        adatag: fromText(''),
        size: fromText('0'),
        rootHash: root_hash,
        mintingPolicy: details.adatagMinting.policyId,
      }

      const newState = Data.to(state, P.StateHolderStateHolder.oldState)
      const assetId = i.toString(16)
      const assetName = AssetName(assetId)

      const mint: TokenMap = new Map()

      mint.set(AssetId.fromParts(policyId, assetName), 1n)
      assets.set(assetName, 1n)
      const value = new Value(0n, mint)

      tx.lockAssets(Address.fromBech32(sateholderAddress), value, newState)
    }

    const txCompleted = await tx
      .provideScript(scripts.authMintingPolicy)
      .addMint(policyId, assets, Data.void())
      .complete()

    const signed = await blaze.wallet.signTransaction(txCompleted, true)
    const ws = txCompleted.witnessSet()
    ws.setVkeys(signed.vkeys()!)
    txCompleted.setWitnessSet(ws)

    const txHash = await blaze.provider.postTransactionToChain(txCompleted)
    await blaze.provider.awaitTransactionConfirmation(txHash)

    const finalDetails = {
      ...details,
      stateholderScript: {
        ...details.stateholderScript,
        refIndex: 0,
      },
      adatagMinting: {
        ...details.adatagMinting,
        refIndex: 1,
      },
      timelockScript: {
        ...details.timelockScript,
        refIndex: 2,
      },
      genesisTransaction: txHash,
    }
    return finalDetails
  }
}
