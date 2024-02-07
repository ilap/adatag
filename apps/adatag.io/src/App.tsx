import { Header } from './components/header/Header'
import { Hero } from './components/hero/Hero'
import { MeshProvider } from "@meshsdk/react";

function App () {
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
