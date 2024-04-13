import { SyncState } from "../services/types"


export interface WorkerContextData {
    setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void
    init(): Promise<void>
    stopFetching(): Promise<void>
    buildTree(newAdatag: string): Promise<any>
}

/*export interface WorkerContextData {
    //initialise(): Promise<void>
    init: () => Promise<any>;
    buildTree(newAdatag: string): Promise<any>
    setSyncStateCallback(callback: ((newState: SyncState) => void) | null): void
}*/

