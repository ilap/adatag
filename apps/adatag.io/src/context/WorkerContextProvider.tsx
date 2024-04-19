import React, { createContext, useContext, useState } from 'react'
import * as Comlink from 'comlink'
import { TreeWorkerService } from '../workers/types'
import TreeWorker from '../workers/TreeWorker?worker'
import { SyncState } from '../services/types'

export const WorkerContext = createContext<TreeWorkerService>(null!)
export const WorkerStateContext = createContext<SyncState>(null!)

type WorkerContextProviderProps = {
  children: React.ReactNode
}

export const useWorker = () => useContext(WorkerContext)
export const useWorkerState = () => useContext(WorkerStateContext)

export const WorkerContextProvider: React.FC<WorkerContextProviderProps> = ({
  children,
}) => {
  const [syncState, setSyncState] = useState<SyncState>({
    state: 'idle',
    message: 'Initialising.',
  })

  // Setup the worker
  const worker = Comlink.wrap<TreeWorkerService>(new TreeWorker())

  return (
    <WorkerContext.Provider value={worker}>
      <WorkerStateContext.Provider value={syncState}>
        {children}
      </WorkerStateContext.Provider>
    </WorkerContext.Provider>
  )
}
