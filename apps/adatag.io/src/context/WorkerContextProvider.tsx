import React, { createContext, useContext, useEffect, useState } from 'react'
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
    const worker = Comlink.wrap<TreeWorkerService>(new TreeWorker());

    // Setup the minting service
    /*const provider = new Kupmios(KUPO_URL, OGMIOS_URL);
    setSloctConfig(NETWORK, ENV);
    const translucent = Translucent.new(provider, Config.network as Network);
    const mintingService = new AdatagMintingService(translucent);
    
    translucent.selectWalletFromSeed(userSeed.seed);
  */


  useEffect(() => {
    /*
    const callback = (newState: SyncState) => {
      setSyncState(newState)
    }

    worker.setSyncStateCallback(Comlink.proxy(callback));

    return () => {
      worker.setSyncStateCallback(null);
    }
    */
  }, []) // No need to include `worker` in the dependency array, as it doesn't change

  return (
    <WorkerContext.Provider value={worker}>
      <WorkerStateContext.Provider value={syncState}>
        {children}
      </WorkerStateContext.Provider>
    </WorkerContext.Provider>
  )
}
