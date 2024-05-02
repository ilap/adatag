import { useLovelace, useNetwork, useWallet, useWalletList } from '@meshsdk/react'
import { useState } from 'react'
import { WalletIcon } from '../../atoms/WalletIcon'
import { Button } from '../../atoms/Button'
import React from 'react'

export const ConnectButton = ({ label, onClick }: { label: string; onClick: () => void }) => {
  const [isEntered, setIsEntered] = useState(false)
  const { connected, connecting, disconnect, name } = useWallet()
  const wallet = useWalletList().find(wallet => wallet.name === name)
  const network = useNetwork()
  const balance = useLovelace()

  const handleMouseEnter = () => setIsEntered(true)
  const handleMouseLeave = () => setIsEntered(false)

  const handleClick = () => (connected ? disconnect() : onClick())

  const renderContent = () => {
    const balanceInADA = Number(parseInt(balance || '0', 10) / 1_000_000)
    const formattedBalance = balanceInADA.toFixed(2)

    return connected ? (
      <div className="flex gap-4 items-center">
        <WalletIcon size="lg" walletName={wallet!.name} icon={wallet!.icon} />
        <p className="max-w-36 min-w-36 text-ellipsis text-center whitespace-nowrap overflow-hidden">
          {isEntered ? 'Disconnect' : `${network === 0 && 't'}₳ ${formattedBalance}`}
        </p>
      </div>
    ) : connecting ? (
      'Connecting'
    ) : (
      `${label}`
    )
  }

  return (
    <div onMouseEnter={handleMouseEnter} onMouseLeave={handleMouseLeave}>
      <Button
        size="xl"
        onClick={handleClick}
        variant={connected ? 'bordered' : 'solid'}
        color={connected ? 'connect' : 'primary'}
        className={`text-foreground ${connected && 'pl-1'} w-full`}>
        {renderContent()}
      </Button>
    </div>
  )
}
