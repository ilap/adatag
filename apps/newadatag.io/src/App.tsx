import React from 'react'
import { Header } from './components/header/Header'
import { Hero } from './components/hero/Hero'
import { MeshProvider } from '@meshsdk/react'

const App: React.FC = () => {
  return (
    <main>
      <MeshProvider>
        <Header />
        <Hero />
      </MeshProvider>
    </main>
  )
}

export default App
