import { useContext } from 'react'
import { Navbar, NavbarBrand, NavbarContent, NavbarItem, Link, } from '@nextui-org/react'

import { AdatagLogo } from '../../../assets/AdatagLogo'

import { DialogContext } from '../../../context/ConnectWalletProvider'
import { ConnectButton } from '../ConnectWallet/ConnectButton'

export default function Header() {
  const { toggleDialog } = useContext(DialogContext);

  const navItems = [
    { label: 'Mint', path: '/' },
    { label: 'Burn', path: '/burn' },
    { label: 'Claim', path: '/claim' },
    { label: 'FAQs', path: '/faq' },
  ];

  return (
    <Navbar
      isBlurred={false}
      isBordered={false}
      maxWidth="full"
      height="h-12"
      className="w-full top-8 z-20 bg-transparent mx-auto p-1  lg:pl-10"
    >
      <NavbarBrand>
        <Link href="/" >
          <AdatagLogo width={161} height={36} />
        </Link>
      </NavbarBrand>
      <NavbarContent
        className="bg-gray-50 hidden lg:rounded-full sm:flex"
        justify="center"
      >
        {navItems.map((item, index) => (
          <NavbarItem key={index} className="p-4" isActive={window.location.pathname === item.path}>
            <Link underline="focus" color="foreground" href={item.path}>
              {item.label}
            </Link>
          </NavbarItem>
        ))}
      </NavbarContent>
      <NavbarContent justify="end">
        <ConnectButton onClick={toggleDialog} label="Connect Wallet" />
      </NavbarContent>
    </Navbar>
  )
}
