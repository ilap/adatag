import React from 'react'
import { Button } from '../../atoms/Button'

import { CustomModal } from '../../molecules/CustomModal'
import { Link, Tooltip } from '@nextui-org/react'
import { DialogContent } from '../../molecules/DialogContent'
import mintErrorIcon from '../../../assets/imgs/err_mint.png'
import successMintIcon from '../../../assets/imgs/succes_mint.png'
import { InformationCircleIcon } from '@heroicons/react/24/outline'

import * as Urls from '../../../configs/expolers-url.json'
import { useConfig } from '../../../hooks/useConfig'

interface Props {
  title: string
  subtitle: string
  isOpen: boolean
  onClose: () => void
  progressResult: string | undefined
  progressError: Error | undefined
}

export const PanelDialog: React.FC<Props> = ({ title, subtitle, isOpen, onClose, progressResult, progressError }) => {
  const config = useConfig()
  const handleClose = () => {
    console.warn(`handleClose`)
    onClose()
  }

  const url = config ? Urls[config.network as keyof typeof Urls].txs : ''

  const renderTooltip = (subTitle: string, tooltip: string) => {
    return (
      <div className="flex">
        {subTitle}
        <Tooltip size="lg" color="foreground" showArrow  content={<div className="text-medium text-wrap max-w-sm whitespace-pre-wrap break-all">{tooltip}</div>}>
          <InformationCircleIcon className="h-5 w-5 cursor-pointer" />
        </Tooltip>
      </div>
    )
  }

  const renderContent = () => {
    return progressError ? (
      <DialogContent
        icon={mintErrorIcon}
        title={title}
        subtitle={renderTooltip(subtitle, (progressError as Error)?.message)}
      />
    ) : (
      progressResult && <DialogContent icon={successMintIcon} title={title} subtitle={subtitle} />
    )
  }

  const renderFooter = () => {
    return progressError ? (
      <Button fullWidth color="primary" onClick={handleClose} size="lg">
        Ok
      </Button>
    ) : (
      progressResult && (
        <Link className="w-full" href={`${url}/${progressResult}`} isExternal>
          <Button fullWidth color="primary" onClick={handleClose} size="lg">
            Track transaction
          </Button>
        </Link>
      )
    )
  }

  return (
    <CustomModal isOpen={isOpen} onClose={handleClose} title="Connect Wallet" footer={renderFooter()}>
      {renderContent()}
    </CustomModal>
  )
}
