import { useLovelace, useWallet, useWalletList } from '@meshsdk/react'
import { useState } from 'react'
import WalletIcon from '../../molecules/WalletIcon'
import { Button } from '../../atoms/Button'

export const ConnectButton = ({
  label,
  onClick,
}: {
  label: string
  onClick: () => void
}) => {
  const [isEntered, setIsEntered] = useState(false)

  const { connected, connecting, disconnect, name } = useWallet()
  const wallet = useWalletList().find(wallet => wallet.name === name)
  const balance = useLovelace()

  const handleMouseEnter = () => {
    setIsEntered(true)
  }

  const handleMouseLeave = () => {
    setIsEntered(false)
  }

  const handleClick = () => {
    if (connected) {
      disconnect()
    } else {
      onClick()
    }
  }

  const renderContent = () => {
    const balanceInADA = Number(parseInt(balance || '0', 10) / 1_000_000)
    const formattedBalance = balanceInADA.toFixed(2)

    return connected ? (
      isEntered ? (
        "Disconnect"
      ) : (
        <div className="flex gap-4 justify-center items-center">
          <WalletIcon size="md" walletName={wallet!.name} icon={wallet!.icon} />
        ₳ {formattedBalance}
        </div>
      )
    ) : connecting ? (
      
      "Connecting"
    ) : (
     `${label}`
    )
  }

  return (
    <div onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
      <Button
        size="xl"
        onClick={handleClick}
        variant={ connected ? 'ghost' : 'solid'}
        className={`text-foreground ${!isEntered && "border-foreground"}`}
      >
        <span
          className="flex  min-w-44   text-ellipsis items-center justify-center"
         // style={{  overflow: 'hidden', textOverflow: 'ellipsis' }}
        >
          {renderContent()}
        </span>
      </Button>
    </div>
  )
}
