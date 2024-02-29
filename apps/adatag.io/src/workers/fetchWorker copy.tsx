import * as Comlink from 'comlink';
import fetch from 'node-fetch';
import StreamArray from 'stream-json/streamers/StreamArray.js';

import { SERVER_URL, BOOTSTRAP_SLOT, SAFETY_SLOTS, POLICY_ID} from './constants';
import { hexToASCII } from '../utils'
import { FetchWorkerApi, QueryWorkerApi } from './types'


class FetchWorker implements FetchWorkerApi {
  db: any

  async init(db: QueryWorkerApi): Promise<any> {
    console.log('%%%%%%%%%%%%%%%%%%%%%%%%%%%% Called....')
    this.db = db
    return true
  }

  async fetchData(): Promise<any[]> {
    try {
      // Fetch tip from the blockchain
      const tip = await this.fetchTip();
      const before = tip.slot_no - SAFETY_SLOTS;

      // Construct query parameters
      const queryParams = [
        "order=oldest_first",
        `created_after=${BOOTSTRAP_SLOT}`,
        `created_before=${before}`
      ].join("&");

      // Construct URL for fetching matches
      const url = `${SERVER_URL}/matches/${POLICY_ID}.*?${queryParams}`;

      // Fetch matches from the blockchain
      const response = await fetch(url);

      // Parse the response using streaming
      const pipeline = response.body.pipe(StreamArray.withParser());

      // Extract relevant data
      const fetchedData: any[] = [];
      pipeline.on('data', chunk => {
        const { address, transaction_index, transaction_id, output_index, value, created_at } = chunk.value;
        const data = value.assets;
        let keysStartingWithPrefix = Object.keys(data).filter(key => key.startsWith(POLICY_ID));
        let tokenName = keysStartingWithPrefix.map(key => key.split(".")[1])[0];
        const assetName = hexToASCII(tokenName);
        fetchedData.push({ address, transaction_index, transaction_id, output_index, value, created_at, assetName });
      });

      // Update database with fetched data
      //await this.db.updateDatabase(fetchedData);

      // Return fetched data
      return []];
    } catch (error) {
      console.error('Error fetching data:', error);
      return [];
    }
  }



  // Function to fetch tip from the blockchain
   async fetchTip(): Promise<any> {
    const response = await fetch(`${SERVER_URL}/checkpoints`);
    const checkpoints = await response.json();
    return checkpoints[0];
  }


  async fetchMetadata(slotNo: number, transactionId: string): Promise<any> {
    return slotNo
  }

  async warnIfNotSynchronized(): Promise<any> {
    return false
  }
}


Comlink.expose(FetchWorker);