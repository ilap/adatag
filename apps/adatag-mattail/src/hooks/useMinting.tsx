import { useContext, useState } from 'react'
import { WorkerContext } from '../context/WorkerContextProvider'
import {
  Kupmios,
  Network,
  SLOT_CONFIG_NETWORK,
  toHex,
  Translucent,
  UTxO,
} from 'translucent-cardano'
import { debugMessage, delay, setSloctConfig, stringifyData } from '../utils'
import { KUPO_URL, OGMIOS_URL, NETWORK, ENV } from '../configs/settings'
import { AdatagMintingService } from '../services/MintingService'
import * as Config from '../configs/genesis-config.json'
import { useAddress, useAssets, useWallet } from '@meshsdk/react'
import { WrappedWallet } from '../services/WrappedApi'
import { user as userSeed } from '../configs/test-users-seed.json'

interface UseMintingResult {
  isMinting: boolean
  mintError: Error | null
  mintResult: any
  mintingProgress: string
  handleMint: (
    adatag: string,
    useAdaHandle: boolean,
    deposit: bigint,
  ) => void
}

const useMinting = (): UseMintingResult => {
  //const { wallet, connected, name, connecting, connect, disconnect, error } = useWallet()
  //const assets = useAssets();
  const address = useAddress()

  const [isMinting, setIsMinting] = useState(false)
  const [mintError, setMintError] = useState<Error | null>(null)
  const [mintResult, setMintResult] = useState<string | null>(null)
  const [mintingProgress, setMintingProgress] = useState<string>('')

  const { createMintingDetails } = useContext(WorkerContext)

  const handleMint = async ( adatag: string, useAdaHandle: boolean, deposit: bigint, ) => {
    try {
      setIsMinting(true)
      setMintingProgress(`Started buildint transaction`)

      const provider = new Kupmios(KUPO_URL, OGMIOS_URL)
      setSloctConfig(NETWORK, ENV)

      const translucent = await Translucent.new(
        provider,
        Config.network as Network,
      )

      // FIXME: implement wallet connect
      //translucent.selectWalletFromSeed(userSeed.seed)
      translucent.selectWalletFromSeed(userSeed.seed)
      const mintingService = new AdatagMintingService(translucent)

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress(`Building base transaction... ${adatag}.... ${useAdaHandle} ..... ${ deposit }`)
      await delay(2000)
      const baseTx = await mintingService.buildBaseTx(
        adatag,
        useAdaHandle,
        address,
        deposit,
      )

      // Step 2. Building the tree's proof from cached and on-chain data.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Creating minting details. pls be patient')
      await delay(2000)
      const { datum, redeemer } = await createMintingDetails(adatag)

      // Step 3. Finalising the tx from the created datum and redeemer.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Finalizing transaction...')
      await delay(2000)
      const finalisedTx = await mintingService.finaliseTx(
        baseTx,
        adatag,
        address,
        datum,
        redeemer,
      )

      // Step 4. Validate the built transaction.
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Completing transaction...')
      await delay(2000)
      const completedTx = await finalisedTx.complete()

      // Step 5. Signing and submitting the validated transaction
      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Signing transaction...')
      await delay(2000)
      const signedTx = await completedTx.sign().complete()

      setMintingProgress('Submitting transaction...')
      await delay(2000)
      ///console.log(toHex(signedTx.txSigned.to_bytes()))

      const txHash = await signedTx.submit()

      console.log(`@@ Submitted TX Hash: ${txHash}...`) // ${stringifyData(signedTx)}`)

      setMintingProgress('Transaction submitted successfully!')
      setMintResult(txHash)
    } catch (error) {
      console.error('Error during minting:', error)
      setMintError(error as Error)
      setMintingProgress('Error during minting. Please check the console for details.',
      )
    } finally {
      setIsMinting(false)
    }
  }

  return { isMinting, mintError, mintResult, mintingProgress, handleMint }
}

export default useMinting
