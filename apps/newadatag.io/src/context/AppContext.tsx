import React, { createContext, useContext, useEffect, useState } from 'react';
import * as Comlink from 'comlink';
import { SQLiteDataStoreService } from '../services/SQLiteDataStore';
import { KupmiosChainFetch } from '../services/KupmiosChainFetch';

//import worker from '../workers/fetchWorker?worker'

interface AppContextState {
    dataStoreService: SQLiteDataStoreService | null;
    chainFetchService: KupmiosChainFetch | null;
}

const AppContext = createContext<AppContextState | undefined>(undefined);

export const useAppContext = () => {
    const context = useContext(AppContext);
    if (!context) {
        throw new Error('useAppContext must be used within an AppContextProvider');
    }
    return context;
};

type StorageProviderProps = { children: React.ReactNode }

export const AppContextProvider: React.FC<StorageProviderProps> = ({  children }) => {
    const [dataStoreService, setDataStoreService] = useState<SQLiteDataStoreService | null>(null);
    const [chainFetchService, setChainFetchService] = useState<KupmiosChainFetch | null>(null);
    //const [fetchStatus, setFetchStatus] = useState<string>('Initializing'); // Example status

    useEffect(() => {
        const initServices = async () => {
            const worker = new Worker(new URL('../workers/fetchWorker.ts', import.meta.url));
            const { dataStoreService, chainFetchService } = Comlink.wrap<{
                dataStoreService: SQLiteDataStoreService
                chainFetchService: KupmiosChainFetch
            }>(worker);

            //setDataStoreService(dataStoreService);
            const fs = await chainFetchService
            setChainFetchService(fs)

            
            //fs.addEventListener((event: any) => {
            //  setFetchStatus(event.detail);
            //});


        };

        initServices();

        // Cleanup function
        return () => {
            // Clean up any resources if needed
        };
    }, []);

    const appContextValue: AppContextState = {
        dataStoreService,
        chainFetchService
    };

    return (
        <AppContext.Provider value={appContextValue}>
            {children}
        </AppContext.Provider>
    );
};
