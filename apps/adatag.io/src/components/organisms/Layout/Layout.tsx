// Layout.tsx
import React, { ReactNode } from 'react'
import { Header } from '../Header/Header'
import { Footer } from '../Footer/Footer'
import { ConnectWalletDialog } from '../ConnectWallet/ConnectWallet'

interface Props {
  children: ReactNode
}

export const Layout: React.FC<Props> = ({ children }) => {
  return (
    <>
      <ConnectWalletDialog />
      <Header />
      <main>{children}</main>
      <Footer />
    </>
  )
}
