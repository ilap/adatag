import { Route, Routes, useLocation, useNavigate } from 'react-router-dom'
import { NextUIProvider } from '@nextui-org/react'
import { useWallet } from '@meshsdk/react'

import { Hero } from './components/organisms/Hero/Hero'
import { FAQ } from './components/organisms/FAQ/Faq'
import { Layout } from './components/organisms/Layout/Layout'

import { LegalPage } from './components/organisms/Legal/LegalPage'
import React, { useEffect } from 'react'

// FIXME: Dirty hack to avoid Buffer undefined in 3rd party tools
import { Buffer } from 'buffer'
window.Buffer = window.Buffer || Buffer

export const App: React.FC = () => {
  const navigate = useNavigate()
  const { connected } = useWallet()
  const location = useLocation()

  useEffect(() => {
    window.scrollTo(0, 0)
  }, [location])

  return (
    <NextUIProvider navigate={(path: string) => {
      console.log(`NAVIGATE: PATH: ${path}`)
      navigate(path, { replace: true })}}>
      <Routes>
        <Route
          path="/"
          element={
            <Layout>
              <Hero connected={connected} headerState={'mint'} />
            </Layout>
          }
        />
        <Route
          path="/claim"
          element={
            <Layout>
              <Hero connected={connected} headerState={'claim'} />
            </Layout>
          }
        />
        <Route
          path="/faq"
          element={
            <Layout>
              <FAQ />
            </Layout>
          }
        />
        <Route
          path="/policy"
          element={
            <Layout>
              <LegalPage type="policy" />
            </Layout>
          }
        />
        <Route
          path="/disclaimer"
          element={
            <Layout>
              <LegalPage type="disclaimer" />
            </Layout>
          }
        />
        <Route
          path="/terms"
          element={
            <Layout>
              <LegalPage type="terms" />
            </Layout>
          }
        />
      </Routes>
    </NextUIProvider>
  )
}
