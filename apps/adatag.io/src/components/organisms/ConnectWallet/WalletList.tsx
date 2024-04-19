import { Listbox, ListboxItem } from '@nextui-org/react'
import { Wallet } from '@meshsdk/core'
import WalletIcon from '../../molecules/WalletIcon'

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


  const capitalise = (name: string) =>
    name.charAt(0).toUpperCase() + name.slice(1)

  //console.log(`##### ${wallets.map(item => item.name)}`)
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
            <WalletIcon size="lg" walletName={item.name} icon={item.icon} />
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
