import React from 'react'
import ReactDOM from 'react-dom/client'
import { WorkerContextProvider } from './StorageProvider.tsx'

import App from './App.tsx'
import './index.css'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <WorkerContextProvider>
      <App />
    </WorkerContextProvider>
  </React.StrictMode>,
)
