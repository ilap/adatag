import React, { useContext, useEffect, useState } from 'react'
import { Wallet } from '@meshsdk/core'
import { useWallet, useWalletList } from '@meshsdk/react'
import { Listbox, ListboxItem, Link, Tooltip } from '@nextui-org/react'

import { CustomModal } from '../../molecules/CustomModal'
import { Button } from '../../atoms/Button'
import { DialogContent } from '../../molecules/DialogContent'
import { InformationCircleIcon } from '@heroicons/react/24/outline'
import { DialogContext } from '../../../context/ConnectWalletProvider'
import { WalletIcon } from '../../atoms/WalletIcon'

import noWalletIcon from '../../../assets/imgs/inf_nowallet.png'
import connecErrorIcon from '../../../assets/imgs/err_wallet.png'

export const ConnectWalletDialog: React.FC = () => {
  const { isDialogOpen, setIsDialogOpen, toggleDialog } = useContext(DialogContext)
  const { connected, error, connecting, connect } = useWallet()
  const wallets = useWalletList()

  const [checkError, setCheckError] = useState(false)

  const handleClose = () => {
    console.warn(`handleClose`)
    setCheckError(false)
    toggleDialog()
  }

  useEffect(() => {
    if (connecting) {
      setCheckError(true)
    }
    if (connected) {
      setIsDialogOpen(false)
      setCheckError(false)
    }
  }, [connecting, connected])

  const renderTooltip = (subTitle: string, tooltip: string) => {
    return (
      <div className="flex">
        {subTitle}
        <Tooltip
          size="lg"
          color="foreground"
          showArrow
          content={
            <div className="text-medium text-wrap overflow-hidden max-w-sm whitespace-pre-wrap break-all">
              {tooltip}
            </div>
          }>
          <InformationCircleIcon className="h-5 w-5 cursor-pointer" />
        </Tooltip>
      </div>
    )
  }
  const renderContent = () => {
    return connecting ? (
      <WalletList disabled={true} wallets={wallets} onConnect={connect} />
    ) : checkError ? (
      <DialogContent
        icon={connecErrorIcon}
        title="Can't connect to wallet"
        subtitle={renderTooltip('Check your installed wallets and try again.', (error as Error)?.message)}
      />
    ) : //(error as Error)?.message
    wallets.length > 0 ? (
      <WalletList disabled={false} wallets={wallets} onConnect={connect} />
    ) : (
      <DialogContent
        icon={noWalletIcon}
        title="No wallets found"
        subtitle={
          <>
            New to browser extension Wallets?&nbsp;
            <Link
              className="font-bold text-foreground"
              size="sm"
              underline="always"
              href="https://cardano-community.github.io/support-faq/Wallets/list/"
              isExternal>
              Get started
            </Link>
          </>
        }
      />
    )
  }

  const renderFooter = () => {
    return connecting ? (
      <Button isLoading color="primary" fullWidth onClick={handleClose} size="lg">
        Connecting
      </Button>
    ) : checkError || wallets.length <= 0 ? (
      <Button fullWidth color="primary" onClick={handleClose} size="lg">
        Ok
      </Button>
    ) : (
      <>
        <div className="text-sm text-center">
          By connecting, you agree to our &nbsp;
          <Link className="font-bold text-gray-800" onClick={handleClose} size="sm" href="/terms" underline="always">
            Terms
          </Link>
          , &nbsp;
          <Link
            className="font-bold text-gray-800"
            onClick={handleClose}
            size="sm"
            href="/disclaimer"
            underline="always">
            Disclaimer
          </Link>
          , and confirm that you have read our &nbsp;
          <Link className="font-bold text-gray-800" onClick={handleClose} size="sm" href="/policy" underline="always">
            Privacy Policy
          </Link>
          .
        </div>
      </>
    )
  }

  return (
    <>
      {isDialogOpen && (
        <CustomModal
          isOpen={isDialogOpen && !connected}
          onClose={handleClose}
          title="Connect Wallet"
          footer={renderFooter()}>
          {renderContent()}
        </CustomModal>
      )}
    </>
  )
}

interface WalletListProps {
  wallets: Wallet[]
  onConnect: (walletName: string) => void
  disabled: boolean
}

export const WalletList: React.FC<WalletListProps> = ({ wallets, onConnect, disabled = false }) => {
  const capitalise = (name: string) => name.charAt(0).toUpperCase() + name.slice(1)

  return (
    <Listbox
      classNames={{
        base: 'max-w-xl',
        list: 'overflow-scroll',
      }}
      items={wallets}
      label="Wallet List"
      variant="flat"
      onAction={key => onConnect(key as string)}
      aria-label="Wallet list"
      disabledKeys={disabled ? wallets.map(item => item.name) : []}>
      {item => (
        <ListboxItem key={item.name} textValue={item.name} className="p-4">
          <div className="flex gap-4 items-center">
            <WalletIcon size="lg" walletName={item.name} icon={item.icon} />
            <div className="flex flex-col">
              <span className="font-medium text-2xl">{capitalise(item.name)}</span>
            </div>
          </div>
        </ListboxItem>
      )}
    </Listbox>
  )
}
