// Define interfaces and types
export interface ChainData {
    tokenName: string
    createdAt: number
}

// Data store service interface
export interface DataStoreService {
    updateDatabase(data: ChainData): Promise<void>
    getLastSlot(): Promise<number> 
}
