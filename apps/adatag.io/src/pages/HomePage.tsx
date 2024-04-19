import React from 'react';
import { useWallet } from '@meshsdk/react';


import Hero from '../components/organisms/Hero/Hero';

const Home: React.FC = () => {
  const { connected } = useWallet()
  
  return (
    <>
      <Hero connected={connected} />
    </>
  );
}

export default Home;
