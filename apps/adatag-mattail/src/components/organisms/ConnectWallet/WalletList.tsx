import { Listbox, ListboxItem } from '@nextui-org/react'
import { walletColors } from './utils'
import { Wallet } from '@meshsdk/core'

interface WalletListProps {
  wallets: Wallet[]
  onConnect: (walletName: string) => void
  disabled: boolean
}

export const WalletList: React.FC<WalletListProps> = ({
  wallets,
  onConnect,
  disabled = false,
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

  const capitalise = (name: string) =>
    name.charAt(0).toUpperCase() + name.slice(1)

  console.log(`##### ${wallets.map(item => item.name)}`)
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
      disabledKeys={disabled ? wallets.map(item => item.name) : []}
    >
      {item => (
        <ListboxItem key={item.name} textValue={item.name} className="p-4">
          <div className="flex gap-4 items-center">
            <div
              className="flex gap-4  flex-shrink-0 items-center justify-center  rounded-full overflow-hidden"
              style={{
                width: 48,
                height: 48,
                backgroundColor: walletColors(item.name),
              }}
            >
              <div
                style={
                  {
                    //mixBlendMode: 'exclusion', // Adjust blend mode as needed
                  }
                }
              >
                {renderIcon(item.icon, 24)}
              </div>
            </div>
            <div className="flex flex-col">
              <span className="font-medium text-2xl">
                {capitalise(item.name)}
              </span>
              {/*<span className="text-small text-default-400">{item.version}</span>*/}
            </div>
          </div>
        </ListboxItem>
      )}
    </Listbox>
  )
}
