/* TODO: Implement generic mintpanel

import useMinting from '../../../hooks/useMinting'
import { AssetInput } from './AssetInput'
import { AssetActionPanel } from './AssetActionPanel'
import { AssetExtended } from '@meshsdk/core'
import { useCallback } from 'react'

export const MintPanel = () => {
  const { handleMint } = useMinting()

  const handleMintAsset = useCallback(
    (asset: AssetExtended | null) => {
      if (asset) {
        handleMint(asset.assetName, true, 10_000n)
      }
    },
    [handleMint]
  )

  return (
    <AssetActionPanel title="Mint adatag" buttonText="Mint adatag" onAction={handleMintAsset}>
      <AssetInput onAssetSelected={handleMintAsset} />
    </AssetActionPanel>
  )
}
*/
