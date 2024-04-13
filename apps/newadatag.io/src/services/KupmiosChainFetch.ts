import { ChainService, ChainStatus, DataStoreService } from './types.ts'
//import { ChainEventBus } from './ChainEventBus';
import { EventTarget } from 'event-target-shim';

export class KupmiosChainFetch implements ChainService {
  private dataStoreService: DataStoreService
  private eventBus: EventTarget
  private status: ChainStatus
  private eventCode: string
  


  constructor(dataStoreService: DataStoreService) {
    this.eventCode = 'KupmiosFetchStatus'
    this.dataStoreService = dataStoreService;
    this.eventBus = new EventTarget();
    this.status = 'error'
  }

  setStatus(newStatus: ChainStatus) {
    this.status = newStatus;
    this.eventBus.trigger('fetchWorkerStatusChange', this.status);
  }

  getStatus() {
    return this.status;
  }

  addEventListener(callback: EventTarget.FallbackEventListener) {
    this.eventBus.addEventListener(this.eventCode, callback)
  }
}