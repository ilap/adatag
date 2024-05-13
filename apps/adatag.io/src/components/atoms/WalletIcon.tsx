import { walletColors } from './utils'
import { iconSizes } from './constants'
import React from 'react'

interface Props {
  walletName: string
  icon: string | undefined
  size: 'sm' | 'md' | 'lg' | 'xl'
}

export const WalletIcon: React.FC<Props> = ({ walletName, icon, size }) => {
  const renderIcon = (icon: string | undefined, size: number) => {
    const filter = `grayscale(1) brightness(2.5)  contrast(3.5)`

    return <img src={icon} width={size} height={size} alt="" style={{ filter, mixBlendMode: 'exclusion' }} />
  }

  return (
    <div
      className="flex gap-4  flex-shrink-0 items-center justify-center  rounded-full overflow-hidden"
      style={{
        width: iconSizes[size].width,
        height: iconSizes[size].width,
        backgroundColor: walletColors(walletName),
      }}>
      <div>{renderIcon(icon, iconSizes[size].iconSize)}</div>
    </div>
  )
}
