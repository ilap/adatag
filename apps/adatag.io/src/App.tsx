import { Route, Routes, useNavigate } from 'react-router-dom'
import { NextUIProvider } from '@nextui-org/react'
import Hero from    './components/organisms/Hero/Hero'
import { FAQ } from './components/organisms/FAQ/Faq'
import Layout from  './components/Layout/Layout'
import { useWallet } from '@meshsdk/react'


export const App: React.FC = () => {
  const navigate = useNavigate()
  const { connected } = useWallet()
  
  return (
    <NextUIProvider navigate={navigate}>
      <Routes>
        <Route path="/" element={<Layout><Hero connected={connected}/></Layout>} />
        <Route path="/faq" element={<Layout><FAQ /></Layout>} />
      </Routes>
    </NextUIProvider>
  )
}
