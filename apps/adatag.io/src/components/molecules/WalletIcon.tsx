import './CustomModal.css'
import { walletColors } from './utils'

interface WalletIconProps {
  walletName: string
  icon: any
  size: 'sm' | 'md' | 'lg' | 'xl'
}

const sizes = {
  sm: {
    width: 24,
    iconSize: 12,
  },
  md: {
    width: 36,
    iconSize: 18,
  },
  lg: {
    width: 48,
    iconSize: 24,
  },
  xl: {
    width: 64,
    iconSize: 32,
  },
}

const WalletIcon: React.FC<WalletIconProps> = ({
  walletName,
  icon = 'lg',
  size,
}) => {
  const renderIcon = (icon: any, size: number) => {
    const filter = ` grayscale(1) brightness(2.5)  contrast(3.5)`

    return (
      <img
        src={icon}
        width={size}
        height={size}
        alt=""
        style={{ filter, mixBlendMode: 'exclusion' }}
      />
    )
  }

  return (
    <div
      className="flex gap-4  flex-shrink-0 items-center justify-center  rounded-full overflow-hidden"
      style={{
        width: sizes[size].width,
        height: sizes[size].width,
        backgroundColor: walletColors(walletName),
      }}
    >
      <div>{renderIcon(icon, sizes[size].iconSize)}</div>
    </div>
  )
}

export default WalletIcon
