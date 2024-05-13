/* TODO: Implement gneeric mintpanel
import { useContext, useEffect, useMemo, useCallback } from 'react'
import { Input } from '../../atoms/Input'
import { useAssets } from '@meshsdk/react'
import { AssetExtended } from '@meshsdk/core'
import useDebouncedSearch, { SearchState } from '../../../hooks/useDebouncedSearch'
import { WorkerContext } from '../../../context/WorkerContextProvider'
import * as Config from '../../../configs/genesis-config.json'
import { getCaption } from './utils'

interface AssetInputProps {
  onAssetSelected: (asset: AssetExtended | undefined) => void
}

export const AssetInput = ({ onAssetSelected }: AssetInputProps) => {
  const assets: AssetExtended[] = useAssets() as unknown as AssetExtended[]
  const { checkIfAdatagMinted } = useContext(WorkerContext)
  const { inputValue, setInputValue, isLoading, searchState, handleChange } = useDebouncedSearch({
    checkIfAdatagMinted,
  })

  const selectedAsset = useMemo(() => {
    if (searchState === SearchState.NotMinted) {
      return assets.find(asset => asset.assetName === inputValue)
    }
    return undefined
  }, [assets, inputValue, searchState])

  useEffect(() => {
    onAssetSelected(selectedAsset)
  }, [onAssetSelected, selectedAsset])

  const isInvalid = useMemo(
    () => searchState === SearchState.Error || searchState === SearchState.InvalidAdatag,
    [searchState]
  )

  return (
    <Input
      disabled={false}
      radius="lg"
      variant="bordered"
      isInvalid={isInvalid}
      size="xl"
      color={searchState === SearchState.Minted ? 'orange' : 'default'}
      onChange={handleChange}
      value={inputValue}
      maxLength={16}
      placeholder="Start typing here..."
      description={isLoading ? 'Checking...' : `${getCaption(searchState)}`}
      errorMessage={isInvalid ? getCaption(searchState) : ''}
    />
  )
}
*/
