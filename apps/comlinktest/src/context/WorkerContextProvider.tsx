import React, { createContext, useContext, useEffect, useState } from 'react';
import * as Comlink from 'comlink';
import { WorkerContextData } from '../workers/types';
import TreeWorker from '../workers/IntegriTreeWorker?worker';
import { SyncState } from '../services/types.ts';

const worker = Comlink.wrap<WorkerContextData>(new TreeWorker());

export const WorkerContext = createContext<WorkerContextData>(null!);

type WorkerStateContextType = {
  syncState: SyncState;
};

export const WorkerStateContext = createContext<WorkerStateContextType>({
  syncState: { state: 'idle', message: '' }
});

type WorkerContextProviderProps = {
  children: React.ReactNode;
};

export const useWorker = () => useContext(WorkerContext)
export const useWorkerState = () => useContext(WorkerStateContext)

export const WorkerContextProvider: React.FC<WorkerContextProviderProps> = ({ children }) => {
  const [syncState, setSyncState] = useState<SyncState>({ state:'idle', message: ''});

  useEffect(() => {

    console.log('Worker instance created', worker); // Verify that the worker instance is being created
    console.log('Worker instance methods', worker); // Verify that the init function is present on the worker instance

    const fetchData = async () => {
      console.log(`$$$$ Initialise Worker`)
      const workerProxy = Comlink.proxy(worker); // Initialize Comlink proxy
  
      await workerProxy.init(); // Call init() on the proxy

      console.log(`$$$$ Initialise Worker`)
    };

    fetchData()

    const callback = (newState: SyncState) => {
      setSyncState(newState);
    };

    worker.setSyncStateCallback(Comlink.proxy(callback));

    return () => {
      worker.setSyncStateCallback(null);
    };
  }, []); // No need to include `worker` in the dependency array, as it doesn't change

  return (
    <WorkerContext.Provider value={ worker }>
    <WorkerStateContext.Provider value={{syncState }}>
      {children}
    </WorkerStateContext.Provider>
    </WorkerContext.Provider>
  );
};
