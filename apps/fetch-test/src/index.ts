import { SQLiteDataStoreService } from "./workers/SQLiteDataStoreService"
import { ContinuousFetchingWorker } from "./workers/ContinuousFetchingWorker"

// Main 
const dataStoreService = new SQLiteDataStoreService();
const continuousFetchingWorker = new ContinuousFetchingWorker(dataStoreService);
continuousFetchingWorker.startFetching();

