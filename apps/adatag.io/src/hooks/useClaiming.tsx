import { useContext, useState } from 'react'
import { WorkerContext } from '../context/WorkerContextProvider'
import { Kupmios, Network, Blaze, WebWallet } from '@blaze-cardano/sdk'

import { setSlotConfig, stringifyData } from '../utils'
import { KUPO_URL, OGMIOS_URL, ENV } from '../configs/settings'
import { AdatagClaimingService } from '../services/ClaimingService'

import { useWallet } from '@meshsdk/react'
import { WalletInstanceWrapper } from '../services/WalletInstanceWrapper'
import { useConfig } from './useConfig'
import { Unwrapped } from '@blaze-cardano/ogmios'
import { TransactionId, TransactionInput } from '@blaze-cardano/core'

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

      const ogmios = await Unwrapped.Ogmios.new(OGMIOS_URL)
      const provider = new Kupmios(KUPO_URL, ogmios)

      await setSlotConfig(config?.network, ENV)

      const wi = wallet._walletInstance
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore Using private property
      const walletWrapper = new WalletInstanceWrapper(wi)
      const webWallet = new WebWallet(walletWrapper)
      const blaze = await Blaze.from(provider, webWallet)

      setProgress(`Getting deposit details`)

      const claimingService = new AdatagClaimingService(blaze)

      const details = await getDepositDetails(adatag)

      if (!details) {
        throw Error(`No deposit was found for the specified adatag: '${adatag}'.`)
      }

      console.warn(`## DEPOSIT DETAILS: ${stringifyData(details)}`)

      const now = Math.floor(Date.now() / 1000) * 1000

      console.warn(`###### DEADLINE: ${now} ${details?.deadline}`)
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

      const inputs: TransactionInput[] = [
        new TransactionInput(TransactionId(details!.txId), BigInt(details!.outputIndex)),
      ]

      const depositUtxos = await blaze!.provider.resolveUnspentOutputs(inputs)

      if (depositUtxos.length === 0) {
        throw Error(`Deposit is already claimed: ${details!.txId}#${details!.outputIndex}.`)
      }

      // Step 1. Creating the base transaction based on the handleUtxo and deposit
      /////////////////////////////////////////////////////////////////////////////

      const baseTx = await claimingService.buildClaimTx('Redeem', details!.beneficiary, donation, depositUtxos)

      /////////////////////////////////////////////////////////////////////////////
      setProgress('Completing transaction')

      const completedTx = await baseTx.complete()

      // Step 5. Signing and submitting the validated transaction
      /////////////////////////////////////////////////////////////////////////////
      setProgress('Signing transaction')

      // DEBUG: console.warn(`### TX: ${completedTx.toCbor().toString()}`)
      const signedTx = await webWallet.signTransaction(completedTx, true)
      const ws = completedTx.witnessSet()
      ws.setVkeys(signedTx.vkeys()!)

      completedTx.setWitnessSet(ws)

      setProgress('Submitting transaction')
      const txHash = await provider.postTransactionToChain(completedTx)

      console.warn(`@@ Submitted TX Hash: ${txHash}\n ... `)
      const isConfirmed = await provider.awaitTransactionConfirmation(txHash)

      setProgress(`Transaction ${isConfirmed ? 'submitted successfully' : 'submission failed'}!`)
      setClaimResult(isConfirmed ? txHash : undefined)
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
