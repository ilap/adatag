import React from 'react'
import { BrowserRouter } from 'react-router-dom'
import { MeshProvider } from '@meshsdk/react'

import { WorkerContextProvider } from './context/WorkerContextProvider.tsx'
import { DialogProvider } from './context/ConnectWalletProvider.tsx'

export function Providers({ children }: { children: React.ReactNode }) {
  return (
    <WorkerContextProvider>
      <MeshProvider>
        <BrowserRouter>
          <DialogProvider>{children}</DialogProvider>
        </BrowserRouter>
      </MeshProvider>
    </WorkerContextProvider>
  )
}
