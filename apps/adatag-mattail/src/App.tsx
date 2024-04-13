import React from 'react'

import Header from './components/organism/Header/Header'
import HeroSection from './components/organism/Hero/Hero'

import { useWallet } from '@meshsdk/react'
const App: React.FC = () => {
  const { connected } = useWallet()
  return (
    <main>
        <Header />
        <HeroSection connected={connected} />
    </main>
  )
}

export default App
