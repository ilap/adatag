import { useContext, useEffect, useMemo, useState } from 'react'
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

export const MintPanel: React.FC = () => {
  const config = useConfig()
  const [isModalOpen, setIsModalOpen] = useState(false)
  const [useHandle, setUseHandle] = useState(false)
  const assets = useAssets() as AssetExtended[]
  const { checkIfAdatagMinted } = useContext(WorkerContext)
  const { inputValue, setInputValue, isLoading, searchState, handleChange } = useDebouncedSearch({
    checkAdatag: checkIfAdatagMinted,
  })
  const { isMinting, mintError, mintResult, handleMint, mintingProgress } = useMinting()

  const rarity = useMemo(() => getRarity(inputValue.length), [inputValue])
  const buttonDisabled = useMemo(
    () => isMinting || isLoading || searchState !== SearchState.NotMinted,
    [isMinting, isLoading, searchState]
  )
  const deposit = useMemo(() => calculateDeposit(inputValue, 1750, 15, 6), [inputValue])
  const formattedDate = useMemo(
    () =>
      new Date(Date.now() + 20 * 86400 * 1000).toLocaleDateString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
      }),
    []
  )

  const adahandle = config && config.adatagMinting.params.adahandle
  const hasAdahandle = useMemo(
    () => assets?.some(asset => asset.assetName === inputValue && asset.policyId === adahandle),
    [assets, inputValue]
  )
  const adahandleChecked = useHandle && hasAdahandle
  const isInvalid = searchState === SearchState.Error || searchState === SearchState.InvalidAdatag

  useEffect(() => {
    if (mintResult) {
      //openModal()
      setIsModalOpen(true)
      setInputValue('')
    } else if (mintError) {
      setIsModalOpen(true)
    }
  }, [mintResult, mintError])

  return (
    <>
      {isModalOpen && (
        <PanelDialog
          isOpen={isModalOpen}
          onClose={() => setIsModalOpen(false)}
          mintError={mintError}
          mintResult={mintResult}
        />
      )}
      <Card className="rounded-xl p-4 max-w-[430px] max-h-[580px]">
        <CardHeader className="flex flex-col gap-3">
          <LoadingSpinner isProgressing={isMinting} content={<>{mintingProgress}</>} />
          <CardHeaderContent
            inputValue={inputValue}
            rarity={rarity}
            useAdahandle={useHandle}
            hasAdahandle={hasAdahandle}
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
