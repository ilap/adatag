import { ChainData, DataStoreService } from "./DataStoreService"
import { ChainService } from "./ChainService"

import { JSONParser } from "@streamparser/json";
import { hexToASCII } from "../utils";

import { SERVER_URL, SAFETY_SLOTS, POLICY_ID } from "../config"

// Continuous Fetching Worker
export class ContinuousFetchingWorker implements ChainService {
    private dataStoreService: DataStoreService;

    constructor(dataStoreService: DataStoreService) {
        this.dataStoreService = dataStoreService;
    }

    // Function to parse response stream
    async #parseResponseStream(response, parser) {
        const reader = response.body.getReader();
        while (true) {
            const { done, value } = await reader.read();
            if (done) break;
            parser.write(value);
        }
    }

    startFetching(): void {
        setInterval(async () => {
            const tip = await this.fetchTip()
            console.log(`Start fetching data at tip: ${JSON.stringify(tip)}`)
            
            // Fetch data from the network starting from last @adatag created, initially from bootstrap time.
            await this.fetchDataChunks();

        }, 5000); 
    }

    async fetchTip(): Promise<number> {
        const response = await fetch(`${SERVER_URL}/checkpoints`);
        const checkpoints = await response.json();
        // Note: checkpoints are ordered by descending slots, so the first item is always the tip
        return checkpoints[0].slot_no;
    }

  /*
  [{
    "transaction_index": 0,
    "transaction_id": "8b05f5d14b64159dcc51c9a512d5e5ea820ec5a3e2d0d3e93d2e5bdabec2610f",
    "output_index": 0,
    "address": "addr_test1qzza6achtargva760zfz8q37wfyl03sjgzev2rtsphew5r0qsh7mgst7chd99j6y6zqf00wx7whmydyjx2tqzxg0vv2qrn4s4m",
    "value": {
      "coins": 1189560,
      "assets": {
        "807b5305cd5c1f6e4d829e6ea92bdb258396f9515f11a9db9b172f32.61646f7a74796a6e6d676474612e6c69": 1
      }
    },
    "datum_hash": null,
    "script_hash": null,
    "created_at": {
      "slot_no": 109,
      "header_hash": "ae186104029b163f217b94bd779fed0671a9b2f822a93c09af228a39d6553120"
    },
    "spent_at": null
  },
  ...
]
*/


    // Fetch data as stream from the chain as chunks
    async fetchDataChunks(): Promise<void> {
        try {
            // Getting the slot nr. of the last @adatag created
            const from = await this.dataStoreService.getLastSlot()

            // Get the tip and some  
            const tip = await this.fetchTip();
            const to = tip - SAFETY_SLOTS;
            
            console.log(`$$$$$ FROM: ${from} TO: ${to}`)
            // Construct query parameters
            const queryParams = [
                "order=oldest_first",
                `created_after=${from}`,
                `created_before=${to}`
            ].join("&");

            // Construct URL for fetching matches
            const url = `${SERVER_URL}/matches/${POLICY_ID}.*?${queryParams}`;
            const response = await fetch(url);

            const parser = new JSONParser({ paths: ["$.*"] });
            
            parser.onValue = async (chunk) => {
                // @ts-ignore: next-line
                const { value, created_at } = chunk.value;
                const data = value.assets;
                
                let keysStartingWithPrefix = Object.keys(data).filter(key => key.startsWith(POLICY_ID));
                // TODO: assuming only one minted at a time.
                let tokenHex = keysStartingWithPrefix.map(key => key.split(".")[1])[0];
                
                const tokenName = hexToASCII(tokenHex);

                // As each chunk arrives, update the database
                const chunkOfData: ChainData = {
                    tokenName: tokenName,
                    createdAt: created_at.slot_no
                }
                await this.dataStoreService.updateDatabase(chunkOfData);
            };
            // Parse response stream
            await this.#parseResponseStream(response, parser);

        } catch (error) {
            console.error("Error:", error);
        }
    }
}
