import React, { useContext, useEffect, useState } from 'react'
import { Button } from '../../atoms/Button'
import { useWallet, useWalletList } from '@meshsdk/react'
import { WalletList } from './WalletList'
import CustomModal from '../../molecules/CustomModal'
import { Link, Tooltip } from '@nextui-org/react'
import { DialogContent } from '../../molecules/DialogContent'
import noWalletIcon from '../../../assets/imgs/inf_nowallet.png'
import connecErrorIcon from '../../../assets/imgs/err_wallet.png'
import { InformationCircleIcon } from '@heroicons/react/24/outline'
import { DialogContext } from '../../../context/ConnectWalletProvider'

export const ConnectWalletDialog: React.FC = () => {

  const { isDialogOpen, setIsDialogOpen,  toggleDialog } = useContext(DialogContext);
  const { connected, error, connecting, connect } = useWallet()
  const wallets = useWalletList()

  const [checkError, setCheckError] = useState(false)

  const handleClose = () => {
    console.warn(`handleClose`)
    setCheckError(false)
    toggleDialog()
  }

  useEffect(() => {
    console.warn(`useeffect conn locell`)
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
          showArrow
          content={<div className="max-w-44 text-small">{tooltip}</div>}
        >
          <InformationCircleIcon className="h-4 w-4 cursor-pointer" />
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
        subtitle={renderTooltip(
          'Check your installed wallets and try again.',
          (error as Error)?.message,
        )}
      />
    ) : //(error as Error)?.message
    wallets.length > 0 ? (
      <WalletList disabled={false} wallets={wallets} onConnect={connect} />
    ) : (
      <DialogContent
        icon={noWalletIcon}
        title="No wallets are found"
        subtitle={
          <>
            New to Wallets?&nbsp;
            <Link
              className="font-bold"
              size="sm"
              underline="always"
              href="https://docs.cardano.org/new-to-cardano/types-of-wallets/"
              isExternal
            >
              Get started
            </Link>
          </>
        }
      />
    )
  }

  const renderFooter = () => {
    return connecting ? (
      <Button isLoading fullWidth onClick={handleClose} size="lg">
        Connecting
      </Button>
    ) : checkError || wallets.length <= 0 ? (
      <Button fullWidth onClick={handleClose} size="lg">
        Ok
      </Button>
    ) : (
      <>
        <div className="text-sm text-center">
          By connecting, you agree to the&nbsp;
          <Link className="font-bold" size="sm" href="#" underline="always">
            Terms of Service
          </Link>
          &nbsp;and
          <br /> have read and acknowledge the &nbsp;
          <Link className="font-bold" size="sm" href="#" underline="always">
            Privacy Policy
          </Link>
          .
        </div>
      </>
    )
  }

  return (
    <>
    { isDialogOpen && (
    <CustomModal
      isOpen={isDialogOpen && !connected}
      onClose={handleClose}
      title="Connect Wallet"
      footer={renderFooter()}
    >
      {renderContent()}
    </CustomModal>
    )}
    </>
  )
}

export default ConnectWalletDialog
