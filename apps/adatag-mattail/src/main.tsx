// main.tsx or main.jsx
import React from 'react'
import ReactDOM from 'react-dom/client'
import { NextUIProvider } from '@nextui-org/react'
import App from './App'
import './index.css'
import { MeshProvider } from '@meshsdk/react'
import { WorkerContextProvider } from './context/WorkerContextProvider.tsx'

ReactDOM.createRoot(document.getElementById('root')!).render(
  //<React.StrictMode>
    <WorkerContextProvider>
      <MeshProvider>
        <NextUIProvider>
          <App />
        </NextUIProvider>
      </MeshProvider>
    </WorkerContextProvider>
  //</React.StrictMode>,
)
