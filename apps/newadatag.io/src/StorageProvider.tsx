import { createContext, useEffect } from 'react';
import * as Comlink from 'comlink';

import QueryWorker from './workers/dbWorker?worker';
import FetchWorker from './workers/fetchWorker?worker';

import { QueryWorkerApi, FetchWorkerApi } from './services/types'; // Import the correct type definition

const queryWorker: QueryWorkerApi = Comlink.wrap<QueryWorkerApi>(new QueryWorker());
const fetchWorker: FetchWorkerApi = Comlink.wrap<FetchWorkerApi>(new FetchWorker())

export const QueryWorkerContext = createContext<QueryWorkerApi>(null!);
export const FetchWorkerContext = createContext<FetchWorkerApi>(null!);

type StorageProviderProps = {
  children: React.ReactNode;
};

export const WorkerContextProvider: React.FC<StorageProviderProps> = ({ children }) => {
  // Initialise the Database and then the Fetch worker....
  useEffect(() => {
    // FIXME: useEffect run twice in dev mode.
    const controller = new AbortController();
    
    queryWorker.init().then(() => {
        console.warn(`XXXXXXXXXXXXXXX Calling init....`)
    });

    fetchWorker.init(queryWorker).then(() => {
      console.warn("############### Fetch is initialised....")
      fetchWorker.fetchData().then(() =>
      {
        console.log(`******** FetchWorker: Finished`)
      })
    })

    
    
    return () => controller.abort(); 
  }, [queryWorker, fetchWorker]);

  return (
    <FetchWorkerContext.Provider value={fetchWorker}>
      <QueryWorkerContext.Provider value={queryWorker}>
        {children}
      </QueryWorkerContext.Provider>
    </FetchWorkerContext.Provider>
    
  )
};
