import {  Navbar,  NavbarBrand,  NavbarContent,  NavbarItem,  Link, } from '@nextui-org/react'
import { useWallet, CardanoWallet } from '@meshsdk/react'

import { Button } from '../../atoms/Button'
import { AdatagLogo } from '../../../assets/AdatagLogo'

import './Header.css'


export default function Header() {
  //const { connected, wallet } = useWallet()

  return (
    <Navbar
      isBlurred={false}
      isBordered={false}
      maxWidth="full"
      height={48}
      className="mx-auto p-1  lg:pl-10"
      style={{
        background: 'transparent',
        position: 'fixed',
        width: '100%',
        top: '30px',
        zIndex: '20',
      }}
    >
      <NavbarBrand>
        <AdatagLogo width={161} height={36} />
      </NavbarBrand>
      <NavbarContent
        className="bg-gray-50 hidden lg:rounded-full sm:flex"
        justify="center"
      >
        <NavbarItem className="p-4">
          <Link color="foreground" href="#">
            Mint
          </Link>
        </NavbarItem>
        <NavbarItem isActive className="p-4">
          <Link href="#" aria-current="page">
            Burn
          </Link>
        </NavbarItem>
        <NavbarItem className="p-4">
          <Link color="foreground" href="#">
            Claim
          </Link>
        </NavbarItem>
        <NavbarItem className="p-4">
          <Link color="foreground" href="#">
            FAQs
          </Link>
        </NavbarItem>
      </NavbarContent>
      <NavbarContent justify="end" className='custom-cardano-wallet'>
      <CardanoWallet label="Connect Wallet"  />
      </NavbarContent>
    </Navbar>
  )
}
