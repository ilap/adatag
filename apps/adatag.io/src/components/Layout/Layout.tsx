// Layout.tsx
import React, { ReactNode } from 'react';
import Header from '../organisms/Header/Header';
import Footer from '../organisms/Footer/Footer';
import ConnectWalletDialog from '../organisms/ConnectWallet/ConnectWallet';


interface LayoutProps {
  children: ReactNode;
}

const Layout: React.FC<LayoutProps> = ({ children }) => {
  return (
    <>
      <ConnectWalletDialog/>
      <Header />
      <main>
        {children}
      </main>
      <Footer />
    </>
  );
}

export default Layout;
