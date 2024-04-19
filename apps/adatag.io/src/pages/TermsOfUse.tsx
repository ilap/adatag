import React from 'react';
import { useWallet } from '@meshsdk/react';

import Layout from '../components/Layout/Layout';
import { FAQ } from '../components/organisms/FAQ/Faq';
import Hero from '../components/organisms/Hero/Hero';

const TermsOfUse: React.FC = () => {
  const { connected } = useWallet()
  
  return (
    <Layout>
      termofuse
      <FAQ />
    </Layout>
  );
}

export default TermsOfUse;
