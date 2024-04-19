import { useLovelace, useWalletList } from '@meshsdk/react';
import WalletIcon from '../../molecules/WalletIcon';

interface WalletBalanceProps {
    connected: boolean
    name: string
    connecting: boolean
    label: string
  }

  export const WalletBalance: React.FC<WalletBalanceProps> = ({
    connected,
    name,
    connecting,
    label,
  }) => {

  const wallet = useWalletList().find((wallet) => wallet.name === name);
  const balance = useLovelace();

  return connected && balance && wallet?.icon ? (
    <>
      <WalletIcon size="md" walletName={wallet.name} icon={wallet.icon} />
      ₳{' '}
      {parseInt((parseInt(balance, 10) / 1_000_000).toString(), 10)}.
      {balance.substring(balance.length - 6)}
    </>
  ) : connected && wallet?.icon ? (
    <>
      <WalletIcon size="lg" walletName={wallet.name} icon={wallet.icon} />
    </>
  ) : connecting ? (
    <>Connecting</>
  ) : (
    <>
      {label}
    </>
  );
};