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
  progressError?: Error
  progressResult?: string
  progress: string
  handleClaim: (adatag: string, donation: bigint) => void
}

export const useClaiming = (): UseClaimingResult => {
  const config = useConfig()
  const { getDepositDetails } = useContext(WorkerContext)

  const { wallet, connected } = useWallet()

  const [isClaiming, setIsClaiming] = useState(false)
  const [progressError, setClaimError] = useState<Error | undefined>(undefined)
  const [progressResult, setClaimResult] = useState<string | undefined>(undefined)
  const [progress, setProgress] = useState<string>('')

  // This is just for redeem by the user
  const handleClaim = async (adatag: string, donation: bigint) => {
    try {
      if (!connected) throw Error(`Wallet is not connected.`)

      setIsClaiming(true)
      setClaimResult(undefined)
      setClaimError(undefined)
      setProgress(`Started building transaction`)

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

      setProgress(`Getting deposit details`)

      const claimingService = new AdatagClaimingService(translucent)

      const details = await getDepositDetails(adatag)

      if (!details) {
        throw Error(`No deposit was found for the specified adatag '${adatag}'.`)
      }

      console.log(`## DEPOSIT DETAILS: ${stringifyData(details)}`)

      const now = Math.floor(Date.now() / 1000) * 1000

      console.log(`###### DEADLINE: ${now} ${details?.deadline}`)
      if (details && now < details?.deadline) {
        throw Error(`Deadline has not passed.
        ${new Date(details?.deadline).toLocaleDateString('en-US', {
          year: 'numeric',
          month: 'short',
          day: 'numeric',
          hour: 'numeric',
          minute: 'numeric',
          timeZoneName: 'shortGeneric',
        })}`)
      }

      const depositUtxos = await translucent!.utxosByOutRef([
        {
          // Deposit UTxOs
          txHash: details!.txId,
          outputIndex: details!.outputIndex,
        },
      ])

      if (depositUtxos.length === 0) {
        throw Error(`Deposit is already claimed: ${details!.txId}#${details!.outputIndex}.`)
      }

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////
      await delay(2000)
      const baseTx = await claimingService.buildClaimTx('Redeem', details!.beneficiary, donation, depositUtxos)

      /////////////////////////////////////////////////////////////////////////////
      setProgress('Completing transaction')
      await delay(2000)
      const completedTx = await baseTx.complete()

      // Step 5. Signing and submitting the validated transaction
      /////////////////////////////////////////////////////////////////////////////
      setProgress('Signing transaction')
      await delay(2000)
      const signedTx = await completedTx.sign().complete()

      setProgress('Submitting transaction')
      await delay(2000)
      ///console.log(toHex(signedTx.txSigned.to_bytes()))

      const txHash = await signedTx.submit()

      console.log(`@@ Submitted TX Hash: ${txHash}...`) // ${stringifyData(signedTx)}`)
      await delay(1000)

      setProgress('Transaction submitted successfully!')
      await delay(2000)
      setClaimResult(txHash)
    } catch (error) {
      console.warn(
        `@@@ ERROR: ${(error as Error)?.name} ... ${typeof error} ${(error as object).toString()} ${stringifyData(
          error
        )}`
      )

      const e = new Error((error as Error)?.message || (error as object).toString())
      setClaimError(e)
      setProgress('Error during claiming. Please check the console for details.')
    } finally {
      setIsClaiming(false)
    }
  }

  return { isClaiming, progressError, progressResult, progress, handleClaim }
}
