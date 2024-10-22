/* TODO: Implement generic mintpanel
import React from 'react'
import { Button } from '../../atoms/Button'

import CustomModal from '../../molecules/CustomModal'
import { Link, Tooltip } from '@nextui-org/react'
import { DialogContent } from '../../molecules/DialogContent'
import mintErrorIcon from '../../../assets/imgs/err_mint.png'
import successMintIcon from '../../../assets/imgs/succes_mint.png'
import { InformationCircleIcon } from '@heroicons/react/24/outline'

import * as Urls from '../../../configs/expolers-url.json'
import * as Config from '../../../configs/genesis-config.json'

interface MintDialogProps {
  isOpen: boolean
  onClose: () => void
  mintResult: string | null
  mintError: Error | null
}

export const PanelDialog: React.FC<MintDialogProps> = ({
  isOpen,
  onClose,
  mintResult,
  mintError,
}) => {
  const handleClose = () => {
    console.warn(`handleClose`)
    onClose()
  }

  const renderTooltip = (subTitle: string, tooltip: string) => {
    return (
      <div className="flex">
        {subTitle}
        <Tooltip showArrow content={<div className="max-w-44 text-small">{tooltip}</div>}>
          <InformationCircleIcon className="h-4 w-4 cursor-pointer" />
        </Tooltip>
      </div>
    )
  }

  const renderContent = () => {
    return mintError ? (
      <DialogContent
        icon={mintErrorIcon}
        title="Can't mint adatag"
        subtitle={renderTooltip(
          'Re-connect the wallet and try again.',
          (mintError as Error)?.message
        )}
      />
    ) : (
      mintResult && (
        <DialogContent
          icon={successMintIcon}
          title="Your adatag is minted"
          subtitle="It should arrive in a minute. Check your wallet's transaction"
        />
      )
    )
  }

  const renderFooter = () => {
    return mintError ? (
      <Button fullWidth onClick={handleClose} size="lg">
        Ok
      </Button>
    ) : (
      mintResult && (
        <Link
          className="w-full"
          href={`${Urls[Config.network as keyof typeof Urls].txs}/${mintResult}`}
          isExternal>
          <Button fullWidth onClick={handleClose} size="lg">
            Track minting transaction
          </Button>
        </Link>
      )
    )
  }

  return (
    <CustomModal
      isOpen={isOpen}
      onClose={handleClose}
      title="Connect Wallet"
      footer={renderFooter()}>
      {renderContent()}
    </CustomModal>
  )
}

export default PanelDialog
*/
