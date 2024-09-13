import { useContext, useState } from 'react'
import { WorkerContext } from '../context/WorkerContextProvider'
import { Kupmios, Data, Blaze, WebWallet } from '@blaze-cardano/sdk'
import { setSlotConfig, stringifyData } from '../utils'
import { KUPO_URL, OGMIOS_URL, ENV } from '../configs/settings'
import { AdatagMintingService } from '../services/MintingService'

import { useAddress, useWallet } from '@meshsdk/react'
import { WalletInstanceWrapper } from '../services/WalletInstanceWrapper'
import { useConfig } from './useConfig'
import { Unwrapped } from '@blaze-cardano/ogmios'
import { PlutusData } from '@blaze-cardano/core'
import { fromText, toText } from '@adatag/common/utils'

import { IntegriTree } from '@adatag/integri-tree'

import {
  TimeDepositDatum,
  StateHolderStateHolder,
  MintRedeemer,
  Operation,
  AdatagAdatagMinting,
  Val,
} from '@adatag/common/plutus'

import { genesisConfig } from '../utils/config'
import { TreeState } from '../services/types'

interface UseMintingResult {
  isMinting: boolean
  progressError: Error | undefined
  progressResult: string | undefined
  mintingProgress: string
  handleMint: (adatag: string, useAdaHandle: boolean, deposit: bigint) => void
  networkId: number
}

const useMinting = (): UseMintingResult => {
  const { wallet, connected } = useWallet()
  const config = useConfig()
  const address = useAddress()

  const [isMinting, setIsMinting] = useState(false)
  const [progressError, setMintError] = useState<Error | undefined>(undefined)
  const [progressResult, setMintResult] = useState<string | undefined>(undefined)
  const [mintingProgress, setMintingProgress] = useState<string>('')

  const { createMintingDetails } = useContext(WorkerContext)

  const networkId = config?.network === 'Mainnet' ? 1 : 0
  const network = config?.network

  //(async () => {
  //  await setSlotConfig(network, ENV)
  //})()

  //const blaze = await Blaze.new(provider, network || 'Custom')

  const handleMint = async (adatag: string, useAdaHandle: boolean, deposit: bigint) => {
    console.warn(`%%%%%%% haliho ${adatag} ${useAdaHandle} ${deposit}`)
    try {
      if (!connected) throw Error(`Wallet is not connected.`)

      setIsMinting(true)
      setMintResult(undefined)
      setMintError(undefined)
      setMintingProgress(`Started building transaction`)

      const ogmios = await Unwrapped.Ogmios.new(OGMIOS_URL)
      const provider = new Kupmios(KUPO_URL, ogmios)

      await setSlotConfig(network, ENV)

      const wi = wallet._walletInstance
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const walletWrapper = new WalletInstanceWrapper(wi)
      const webWallet = new WebWallet(walletWrapper)
      const blaze = await Blaze.from(provider, webWallet)

      //blaze.selectWallet()
      //blaze.selectWalletFromSeed(userSeed.seed)
      //blaze.selectWalletFromSeed(userSeed.seed)

      const mintingService = new AdatagMintingService(blaze)
      console.warn(`%%%%%%% MINTIGSERVICE`)

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress(`Building transaction`) // ${adatag}.... ${useAdaHandle} ..... ${ deposit }`)

      const baseTx = await mintingService.buildBaseTx(adatag, useAdaHandle, address, deposit)

      // Step 2. Building the tree's proof from cached and on-chain data.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Creating minting details')

      // FIXME: refactored `createMintingDetails` to preserve type information when
      // passing data between worker thread and main thread. The PlutusData type is not preserved in the worker (as of now).
      const { datum, redeemer } = await createMintingDetails(adatag)
      const mintDatum = Data.to(datum, StateHolderStateHolder.oldState)
      const mintRedeemer = Data.to(redeemer, AdatagAdatagMinting.rdmr, 'proof')

      // Step 3. Finalising the tx from the created datum and redeemer.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Finalizing transaction')

      const finalisedTx = await mintingService.finaliseTx(baseTx, adatag, address, mintDatum, mintRedeemer)

      // Step 4. Validate the built transaction.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Completing transaction')

      const completedTx = await finalisedTx.complete()

      // Step 5. Signing and submitting the validated transaction
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Signing transaction')

      console.warn(`### TX: ${completedTx.toCbor().toString()}`)
      const signedTx = await webWallet.signTransaction(completedTx, true)
      const ws = completedTx.witnessSet()
      ws.setVkeys(signedTx.vkeys()!)
      completedTx.setWitnessSet(ws)

      setMintingProgress('Submitting transaction')
      const txHash = await provider.postTransactionToChain(completedTx)

      console.warn(`@@ Submitted TX Hash: ${txHash}\n ... `)
      const isConfirmed = await provider.awaitTransactionConfirmation(txHash)

      setMintingProgress(`Transaction ${isConfirmed ? 'submitted successfully' : 'submission failed'}!`)
      // await delay(2000)
      setMintResult(isConfirmed ? txHash : undefined)
    } catch (error) {
      console.warn(`@@@ ERROR: ${error} ... ${typeof error} ${(error as object).toString()} ${stringifyData(error)}`)

      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const e = new Error('Error during minting:' + (error as any)?.toString())
      setMintError(e)
      setMintingProgress('Error during minting. Please check the console for details.')
    } finally {
      setIsMinting(false)
    }
  }

  return {
    isMinting,
    progressError,
    progressResult,
    mintingProgress,
    handleMint,
    networkId,
  }
}

export default useMinting
