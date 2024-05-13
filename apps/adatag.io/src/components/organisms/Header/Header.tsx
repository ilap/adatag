import React, { useContext } from 'react'
import { NavbarContent, NavbarItem, Link, Navbar, NavbarBrand } from '@nextui-org/react'

import { useWallet } from '@meshsdk/react'
import { AdatagLogo } from '../../../assets/AdatagLogo'
import { ConnectButton } from '../ConnectWallet/ConnectButton'
import { DialogContext } from '../../../context/ConnectWalletProvider'

export const Header: React.FC = () => {
  const { connected } = useWallet()
  const { toggleDialog } = useContext(DialogContext)

  const navItems = [
    { label: 'Mint', path: '/' },
    // TODO: Implement burn adatag
    // { label: 'Burn', path: '/burn' },
    { label: 'Claim', path: '/claim' },
    { label: 'FAQs', path: '/faq' },
  ]

  return (
    <Navbar
      isBlurred={false}
      isBordered={false}
      maxWidth="full"
      height="h-12"
      className="w-full top-8 z-20 bg-transparent mx-auto p-1  lg:pl-10">
      <NavbarBrand>
        <Link href="/">
          <AdatagLogo width={161} height={48} />
        </Link>
      </NavbarBrand>
      <NavbarContent className="bg-neutral-50 pl-6 pr-6 hidden lg:rounded-full sm:flex" justify="center">
        {navItems.map((item, index) => {
          const isFaq = item.path === '/faq'
          const isActive = window.location.pathname === item.path && (connected || isFaq)

          return (
            <NavbarItem
              key={index}
              className={`p-4 ${!isFaq && !connected && 'cursor-not-allowed opacity-50'}`}
              isActive={isActive}>
              <Link
                isDisabled={!connected && item.path !== '/faq'}
                underline={isActive ? 'always' : 'none'}
                color="foreground"
                href={item.path}
                className="text-xl font-medium">
                {item.label}
              </Link>
            </NavbarItem>
          )
        })}
      </NavbarContent>
      <NavbarContent justify="end">
        <ConnectButton onClick={toggleDialog} label="Connect Wallet" />
      </NavbarContent>
    </Navbar>
  )
}
