import { useContext, useState } from 'react'
import { WorkerContext } from '../context/WorkerContextProvider'
import { Kupmios, Network, Translucent } from 'translucent-cardano'

import { delay, setSlotConfig, stringifyData } from '../utils'
import { KUPO_URL, OGMIOS_URL, ENV } from '../configs/settings'
import { AdatagClaimingService } from '../services/ClaimingService'

import { useWallet } from '@meshsdk/react'
import { WalletInstanceWrapper } from '../services/WalletInstanceWrapper'
import { useConfig } from './useConfig'

interface UseClaimingResult {
  isClaiming: boolean
  claimError?: Error
  claimResult?: string
  mintingProgress: string
  handleClaim: (adatag: string, donation: bigint) => void
}

export const useClaiming = (): UseClaimingResult => {
  const config = useConfig()
  const { getDepositDetails } = useContext(WorkerContext)

  const { wallet, connected } = useWallet()

  const [isClaiming, setIsClaiming] = useState(false)
  const [claimError, setClaimError] = useState<Error | undefined>(undefined)
  const [claimResult, setClaimResult] = useState<string | undefined>(undefined)
  const [mintingProgress, setMintingProgress] = useState<string>('')

  // This is just for redeem by the user
  const handleClaim = async (adatag: string, donation: bigint) => {
    try {
      if (!connected) throw Error(`Wallet is not connected.`)

      setIsClaiming(true)
      setClaimResult(undefined)
      setClaimError(undefined)
      setMintingProgress(`Started building transaction`)

      const provider = new Kupmios(KUPO_URL, OGMIOS_URL)
      const network = config && (config.network as Network)
      setSlotConfig(network || 'Custom', ENV)

      const translucent = await Translucent.new(provider, network || 'Custom')

      // WallettConnect wrapper
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const wi = wallet._walletInstance
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const walletWrapper = new WalletInstanceWrapper(wi)
      translucent.selectWallet(walletWrapper)
      //translucent.selectWalletFromSeed(userSeed.seed)

      setMintingProgress(`Getting deposit details`)

      const claimingService = new AdatagClaimingService(translucent)

      const details = await getDepositDetails(adatag)

      const now = Math.floor(Date.now() / 1000) * 1000

      console.log(`###### DEADLINE: ${now} ${details?.deadline}`)
      if (details && now < details?.deadline) {
        throw Error(`Deadline has not passed. ${now.toLocaleString()}`)
      }

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////
      await delay(2000)
      const baseTx = await claimingService.buildClaimTx(
        'Redeem',
        details!.beneficiary,
        donation,
        details!.txId,
        details!.outputIndex
      )

      /////////////////////////////////////////////////////////////////////////////
      setMintingProgress('Completing transaction')
      await delay(2000)
      const completedTx = await baseTx.complete()

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
      setClaimResult(txHash)
    } catch (error) {
      console.warn(`@@@ ERROR: ${error} ... ${typeof error} ${(error as object).toString()} ${stringifyData(error)}`)

      const e = new Error('Error during claiming:' + (error as object)?.toString())
      setClaimError(e)
      setMintingProgress('Error during claiming. Please check the console for details.')
    } finally {
      setIsClaiming(false)
    }
  }

  return { isClaiming, claimError, claimResult, mintingProgress, handleClaim }
}
