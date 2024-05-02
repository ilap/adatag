import { useContext, useState } from 'react'
import { WorkerContext } from '../context/WorkerContextProvider'
import { Kupmios, Network, Translucent } from 'translucent-cardano'
import { delay, setSlotConfig, stringifyData } from '../utils'
import { KUPO_URL, OGMIOS_URL, ENV } from '../configs/settings'
import { AdatagMintingService } from '../services/MintingService'

import { useAddress, useWallet } from '@meshsdk/react'
import { WalletInstanceWrapper } from '../services/WalletInstanceWrapper'
import { useConfig } from './useConfig'

interface UseMintingResult {
  isMinting: boolean
  mintError: Error | undefined
  mintResult: string | undefined
  mintingProgress: string
  handleMint: (adatag: string, useAdaHandle: boolean, deposit: bigint) => void
  networkId: number
}

const useMinting = (): UseMintingResult => {
  const { wallet, connected } = useWallet()
  const config = useConfig()
  const address = useAddress()

  const [isMinting, setIsMinting] = useState(false)
  const [mintError, setMintError] = useState<Error | undefined>(undefined)
  const [mintResult, setMintResult] = useState<string | undefined>(undefined)
  const [mintingProgress, setMintingProgress] = useState<string>('')

  const { createMintingDetails } = useContext(WorkerContext)

  const networkId = config?.network === 'Mainnet' ? 1 : 0
  const network = config?.network as Network

  //(async () => {
  //  await setSlotConfig(network, ENV)
  //})()

  //const translucent = await Translucent.new(provider, network || 'Custom')

  const handleMint = async (adatag: string, useAdaHandle: boolean, deposit: bigint) => {
    try {
      if (!connected) throw Error(`Wallet is not connected.`)

      setIsMinting(true)
      setMintResult(undefined)
      setMintError(undefined)
      setMintingProgress(`Started building transaction`)

      const provider = new Kupmios(KUPO_URL, OGMIOS_URL)
      await setSlotConfig(network, ENV)

      const translucent = await Translucent.new(provider, network)

      // FIXME: implement wallet connect
      //
      //const walletApi = await getSelectedWallet();
      //            translucent.selectWallet(walletApi);
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const wi = wallet._walletInstance
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const walletWrapper = new WalletInstanceWrapper(wi)
      translucent.selectWallet(walletWrapper)
      //translucent.selectWallet()
      //translucent.selectWalletFromSeed(userSeed.seed)
      //translucent.selectWalletFromSeed(userSeed.seed)

      const mintingService = new AdatagMintingService(translucent)

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress(`Building transaction`) // ${adatag}.... ${useAdaHandle} ..... ${ deposit }`)
      //await delay(2000)
      const baseTx = await mintingService.buildBaseTx(adatag, useAdaHandle, address, deposit)

      // Step 2. Building the tree's proof from cached and on-chain data.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Creating minting details')
      //await delay(2000)
      const { datum, redeemer } = await createMintingDetails(adatag)

      // Step 3. Finalising the tx from the created datum and redeemer.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Finalizing transaction')
      await delay(2000)
      const finalisedTx = await mintingService.finaliseTx(baseTx, adatag, address, datum, redeemer)

      // Step 4. Validate the built transaction.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Completing transaction')
      await delay(2000)
      const completedTx = await finalisedTx.complete()

      // Step 5. Signing and submitting the validated transaction
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Signing transaction')
      await delay(2000)
      const signedTx = await completedTx.sign().complete()

      setMintingProgress('Submitting transaction')
      await delay(2000)
      ///console.log(toHex(signedTx.txSigned.to_bytes()))

      const txHash = await signedTx.submit()

      console.log(`@@ Submitted TX Hash: ${txHash}...`) // ${stringifyData(signedTx)}`)
      await delay(1000)

      setMintingProgress('Transaction submitted successfully!')
      await delay(2000)
      setMintResult(txHash)
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
    mintError,
    mintResult,
    mintingProgress,
    handleMint,
    networkId,
  }
}

export default useMinting
