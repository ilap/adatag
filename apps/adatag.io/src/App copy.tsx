import React from 'react'

import Layout from './components/Layout/Layout'
import Home from './pages/HomePage'

const App: React.FC = () => {
  
  return (
    <main>

        <Layout>
        {/*<Header />*/}
        {/*<HeroSection connected={connected} />*/}
        {/*<FAQ />*/}
          <Home />
        </Layout>
    </main>
  )
}

export default App
