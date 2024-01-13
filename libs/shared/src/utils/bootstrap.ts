import {
  Script,
  Data,
  Assets,
  Translucent,
  UTxO,
  fromText,
} from 'translucent-cardano'
import * as P from '../plutus/plutus'
import { hash256, emptyHash, Val, combineThreeHashes, hashVal }  from '@adatag/integri-tree'
//import sha256 from 'sha256'
//import blake2b_224 from 'blake2b'
import { GenesisConfig, GenesisParams } from '../config/types'

const emptyBlake2b224 = [
  0x83, 0x6c, 0xc6, 0x89, 0x31, 0xc2, 0xe4, 0xe3, 0xe8, 0x38, 0x60, 0x2e, 0xca,
  0x19, 0x02, 0x59, 0x1d, 0x21, 0x68, 0x37, 0xba, 0xfd, 0xdf, 0xe6, 0xf0, 0xc8,
  0xcb, 0x07,
]

const emptySha2256 = [
  0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99,
  0x6f, 0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95,
  0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
]

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
    translucent: Translucent,
    deployerUtxo: UTxO,
    params: GenesisParams,
  ): Promise<GenesisConfig> {
    const result = await this.deploy(translucent, deployerUtxo, params)
    Bun.write(Bun.file(file), JSON.stringify(result, null, '  '))
    return result
  }

  public static async deploy(
    translucent: Translucent,
    deployerUtxo: UTxO,
    params: GenesisParams,
  ): Promise<GenesisConfig> {
    const epoch = Date.now()
    const defaultDetails: GenesisConfig = {
      ...this.defaultDetails,
      network: translucent.network,
      hashAlg: params.hashAlg,
      bootstrapTime: {
        epoch: epoch,
        date: new Date(epoch).toString(),
      },
    }

    const refDetails = this.getReferenceDetails(
      translucent,
      params,
      defaultDetails,
    )
    const { authMintingPolicy, authDetails } = this.getAuthTokenScript(
      translucent,
      deployerUtxo,
      refDetails,
    )
    const { timelockScript, timelockDetails } = this.getTimelockDetails(
      translucent,
      params,
      authDetails,
    )
    const { mintingPolicy, stateholderValidator, mintingDetails } =
      this.getMintingDetails(translucent, params, timelockDetails)

    return await this.buildTx(translucent, deployerUtxo, mintingDetails, {
      authMintingPolicy,
      timelockScript,
      stateholderValidator,
      mintingPolicy,
    })
  }

  private static getReferenceDetails(
    translucent: Translucent,
    params: GenesisParams,
    details: GenesisConfig,
  ) {
    const referenceScript = new P.AlwaysFailAlwaysFail()
    const alwaysFailHash =
      translucent.utils.validatorToScriptHash(referenceScript)
    const referenceAddress = translucent.utils.credentialToAddress(
      translucent.utils.scriptHashToCredential(alwaysFailHash),
    )

    return {
      ...details,
      referenceScript: {
        scriptAddress: referenceAddress,
        scriptHash: alwaysFailHash,
      },
    }
  }

  private static getAuthTokenScript(
    translucent: Translucent,
    deployerUtxo: UTxO,
    details: GenesisConfig,
  ) {
    const authMintingPolicy = new P.OneshotAuthToken({
      transactionId: { hash: deployerUtxo.txHash },
      outputIndex: BigInt(deployerUtxo.outputIndex),
    })
    const policyId = translucent.utils.mintingPolicyToId(authMintingPolicy)

    return {
      authMintingPolicy,
      authDetails: {
        ...details,
        authTokenScript: {
          policyId: policyId,
          params: {
            genesis_utxo: {
              txHash: deployerUtxo.txHash,
              outputIndex: deployerUtxo.outputIndex,
            },
          },
        },
      },
    }
  }

  private static getTimelockDetails(
    translucent: Translucent,
    params: GenesisParams,
    details: GenesisConfig,
  ) {
    const collectionTime =
      details.bootstrapTime.epoch + this.days(params.collectionTime.toString())
    const addr = translucent.utils.getAddressDetails(params.collectorAddress)
    const timeLock = new P.TimeDepositTimedeposit({
      collector: addr.paymentCredential!.hash,
      collectionTime: BigInt(collectionTime),
    })
    const timeLockHash = translucent.utils.validatorToScriptHash(timeLock)
    const timelockAddress = translucent.utils.credentialToAddress(
      translucent.utils.scriptHashToCredential(timeLockHash),
    )

    return {
      timelockScript: timeLock,
      timelockDetails: {
        ...details,
        timelockScript: {
          scriptHash: timeLockHash,
          scriptAddress: timelockAddress,
          params: {
            collectorAddr: addr.address.bech32,
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

  private static getMintingDetails(
    translucent: Translucent,
    params: GenesisParams,
    details: GenesisConfig,
  ) {
    const authPolicyId = details.authTokenScript.policyId
    const stateholderValidator = new P.StateHolderStateHolder(authPolicyId)

    const stateholderHash =
      translucent.utils.validatorToScriptHash(stateholderValidator)
    const stateholderCredential =
      translucent.utils.scriptHashToCredential(stateholderHash)
    const stateholderAddress = translucent.utils.credentialToAddress(
      stateholderCredential,
    )

    const deactivationTime =
      details.bootstrapTime.epoch +
      this.days(params.deactivationTime.toString())
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

    const mintingPolicy = new P.AdatagAdatagMinting(scriptPparams)
    const mintingPolicyId = translucent.utils.mintingPolicyToId(mintingPolicy)

    return {
      mintingPolicy,
      stateholderValidator,
      mintingDetails: {
        ...details,
        stateholderScript: {
          scriptHash: stateholderHash,
          scriptAddress: stateholderAddress,
          params: {
            authToken: details.authTokenScript.policyId,
          },
          refIndex: -1,
        },
        adatagMinting: {
          policyId: mintingPolicyId,
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
    translucent: Translucent,
    deployerUtxO: UTxO,
    details: GenesisConfig,
    scripts: {
      authMintingPolicy: Script
      timelockScript: Script
      stateholderValidator: Script
      mintingPolicy: Script
    },
  ) {
    const refAddr = details.referenceScript.scriptAddress
    const tx = translucent
      .newTx()
      .collectFrom([deployerUtxO])
      .payToContract(
        refAddr,
        { inline: Data.void(), scriptRef: scripts.stateholderValidator },
        {},
      )
      .payToContract(
        refAddr,
        { inline: Data.void(), scriptRef: scripts.mintingPolicy },
        {},
      )
      .payToContract(
        refAddr,
        { inline: Data.void(), scriptRef: scripts.timelockScript },
        {},
      )

    const assets: Assets = {}
    const sateholderAddress = details.stateholderScript.scriptAddress

    for (let i = 97; i <= 122; i++) {
      // const val = [48, i - 1, i + 1]
      // FIXME: The hash alg should be defined only in integri-tree module
      const hash_alg = details.hashAlg
      let root_hash

      const val:Val = { 
        xi: String.fromCharCode(48),
        xa: String.fromCharCode(i-1),
        xb: String.fromCharCode(i+1)
      }

      const hv = hashVal(val)
      root_hash = combineThreeHashes(hv, emptyHash, emptyHash)


      const state: P.StateHolderStateHolder['oldState'] = {
        operationCount: 0n,
        operation: 'AdatagRemoved',
        adatag: fromText(''),
        size: fromText('0'),
        rootHash: root_hash,
        mintingPolicy: details.adatagMinting.policyId,
      }

      const id = Data.to(state, P.StateHolderStateHolder.oldState)

      const assetId = details.authTokenScript.policyId + i.toString(16)
      assets[assetId] = 1n
      tx.payToContract(sateholderAddress, { inline: id }, { [assetId]: 1n })
    }

    const txCompleted = await tx
      .attachMintingPolicy(scripts.authMintingPolicy)
      .mintAssets(assets, Data.void())
      .complete()

    const signedTx = await txCompleted.sign().complete()

    // DEBUG: console.log(`\n\n Transaction: \n ${signedTx.txSigned.to_json()}`)
    const txHash = await signedTx.submit()
    await translucent.awaitTx(txHash)
    // DEBUG:  console.log(`TxHash ${txHash}`)

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
