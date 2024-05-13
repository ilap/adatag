import { useContext, useEffect, useState } from 'react'
import { Card, CardHeader, CardBody, CardFooter } from '@nextui-org/react'
import { useAssets } from '@meshsdk/react'
import { Button } from '../../atoms/Button'
import { Input } from '../../atoms/Input'
import useMinting from '../../../hooks/useMinting'
import { calculateDeposit, getCaption, getRarity } from './utils'
import { checkingCaption, mintguideline } from './constants'
import { WorkerContext } from '../../../context/WorkerContextProvider'
import useDebouncedSearch, { SearchState } from '../../../hooks/useDebouncedSearch'
import { PanelDialog } from './PanelDialog'
import { AssetExtended } from '@meshsdk/core'
import { LoadingSpinner } from './LoadingSpinner'
import { CardHeaderContent } from './CardHeaderContent'
import { useConfig } from '../../../hooks/useConfig'
import React from 'react'

export const MintPanel: React.FC = () => {
  const config = useConfig()
  const [isModalOpen, setIsModalOpen] = useState(false)
  const [useHandle, setUseHandle] = useState(false)
  const assets = useAssets() as AssetExtended[]
  const { checkIfAdatagMinted } = useContext(WorkerContext)
  const { inputValue, setInputValue, isLoading, searchState, handleChange } = useDebouncedSearch({
    checkAdatag: checkIfAdatagMinted,
  })
  const { isMinting, progressError, progressResult, handleMint, mintingProgress } = useMinting()

  const rarity = getRarity(inputValue.length)
  const buttonDisabled = isMinting || isLoading || searchState !== SearchState.NotMinted
  const now = Date.now()

  const deactivated = config && config.adatagMinting.params.deactivationTime.epoch < now

  const deposit = deactivated ? 0n : calculateDeposit(inputValue, 1750, 15, 6)

  const lockingDays = config?.adatagMinting.params.lockingDays.days || 0
  const formattedDate = deactivated
    ? 'Deactivated'
    : new Date(now + lockingDays * 86400 * 1000).toLocaleDateString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
      })

  const adahandle = config && config.adatagMinting.params.adahandle
  const hasAdahandle = assets?.some(asset => asset.assetName === inputValue && asset.policyId === adahandle)
  const adahandleChecked = useHandle && hasAdahandle
  const isInvalid = searchState === SearchState.Error || searchState === SearchState.InvalidAdatag

  useEffect(() => {
    if (progressResult) {
      setIsModalOpen(true)
      setInputValue('')
    } else if (progressError) {
      setIsModalOpen(true)
    }
  }, [progressResult, progressError])

  return (
    <>
      {isModalOpen && (
        <PanelDialog
          title={progressResult ? 'Your adatag is minted!' : "Can't mint adatag"}
          subtitle={
            progressResult
              ? "It should arrive in a minute. Check your wallet's transaction"
              : 'Re-connect the wallet and try again.'
          }
          isOpen={isModalOpen}
          onClose={() => setIsModalOpen(false)}
          progressError={progressError}
          progressResult={progressResult}
        />
      )}
      <Card className="rounded-xl p-4 max-w-[430px] max-h-[580px]">
        <CardHeader className="flex flex-col gap-3">
          <LoadingSpinner isProgressing={isMinting} content={<p className="text-center">{mintingProgress}</p>} />
          <CardHeaderContent
            inputValue={inputValue}
            rarity={rarity}
            useAdahandle={deactivated ? false : useHandle}
            hasAdahandle={deactivated ? false : hasAdahandle}
            adahandleChecked={adahandleChecked}
            deposit={deposit}
            formattedDate={formattedDate}
            onToggleAdahandle={() => setUseHandle(!useHandle)}
          />
        </CardHeader>
        <CardBody>
          <Input
            disabled={isMinting}
            radius="lg"
            variant="bordered"
            isInvalid={isInvalid}
            size="xl"
            color={searchState === SearchState.Minted ? 'orange' : 'default'}
            onChange={handleChange}
            value={inputValue}
            maxLength={16}
            placeholder="Type to search..."
            description={isLoading ? checkingCaption : getCaption(searchState)}
            errorMessage={isInvalid ? getCaption(searchState) : ''}
          />
          <Button
            size="lg"
            color="primary"
            onPress={() => handleMint(inputValue, adahandleChecked, deposit)}
            className={`${buttonDisabled ? 'cursor-not-allowed' : 'cursor-pointer'}`}
            isDisabled={buttonDisabled}>
            Mint adatag
          </Button>
        </CardBody>
        <CardFooter>
          <p className="text-small text-center text-default-500 m-0">{mintguideline}</p>
        </CardFooter>
      </Card>
    </>
  )
}
