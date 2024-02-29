export interface QueryWorkerApi {
    db: any
    initialising: any
    init: () => Promise<any>
    query: (sql: string, bind: (string | number)[]) => Promise<any>
    select: (table: string) => any
    updateDatabase: (transactions: any[]) => any
    cleanup: () => Promise<any>
}

// Define interface for API operations
export interface FetchWorkerApi {
    init(db: QueryWorkerApi): Promise<any>;
    fetchData(): Promise<void>;
    fetchMetadata(slotNo: number, transactionId: string): Promise<any>;
    fetchTip(): Promise<any>;
    warnIfNotSynchronized(): Promise<any>;
}
