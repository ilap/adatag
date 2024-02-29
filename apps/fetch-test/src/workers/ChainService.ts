// Chain service interface
export interface ChainService {
    fetchTip(): Promise<number>
    fetchDataChunks(): Promise<void>
}
