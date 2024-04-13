import { KupmiosChainFetch } from '../services/KupmiosChainFetch';
import { SQLiteDataStoreService } from '../services/SQLiteDataStore';

import * as Comlink from 'comlink';

// Create an instance of SQLiteDataStoreService
const dataStoreService = new SQLiteDataStoreService();

// Create an instance of KupmiosChainFetch with the dataStoreService instance
const chainFetchService = new KupmiosChainFetch(dataStoreService);

// Expose both instances using Comlink
Comlink.expose({
  dataStoreService,
  chainFetchService
});
