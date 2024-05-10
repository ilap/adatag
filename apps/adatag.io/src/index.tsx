import ReactDOM from 'react-dom/client'
import { WorkerContextProvider } from './context/WorkerContextProvider.tsx'

import App from './App.tsx'
import './index.css'
import React from 'react'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <WorkerContextProvider>
    <App />
  </WorkerContextProvider>,
)
