# adatag.io

Vite + React + TailwindCSS PoC for adatag.io

```
git clone https://github.com/ilap/adatag && cd adatag
nx run @adatag/adatag.io:serve:custom
```

> Warning: This solution uses public endpoints from **_demeter.run_**, which could potentially be misused by an adversary. Please be cautious when using this method.

To deploy an update, modify the `apps/adatag.io/src/configs/settings.ts` file with the appropriate plublic endpoints from `demeter.run`. For example:

```typescript
export const KUPO_URL = 'http://localhost:1442'
export const OGMIOS_URL = 'ws://localhost:1337'
export const ENV = 'Integration'
```

> Note: Ensure that the configuration settings are consistent with the generated `genesis-config.json` file.

# Issues

## blaze-cardano

### Wrongly generated dist files

After `pnpm build` it generates the following code under the `blaze-ogmios/dist/index.mjs

```
21 __reExport(schema_exports, schema_star);
22 import * as schema_star from "@cardano-ogmios/schema";
```

```
21 import * as schema_star from "@cardano-ogmios/schema";
22 __reExport(schema_exports, schema_star);
```

### Isomorphic-ws issue

It was only compatible with Node.js

```typescript
import { WebSocket } from "isomorphic-ws";
import type * as schema from "./schema";

export class Ogmios {
  private ws: WebSocket;
  private url: string;
  private requests: Record<
    string,
    { resolve: (value: any) => void; reject: (reason: any) => void }
  > = {};

  private constructor(url: string) {
    console.log("url", url);
    this.url = url;
    this.ws = new WebSocket(this.url);
  }

  private async setupEventListeners(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.ws.onopen = () => {
        resolve();
      };

      this.ws.onmessage = (event) => {
        const messageText =
          typeof event.data === "string"
            ? event.data
            : event.data.toString("utf-8");
        const parsedMessage = JSON.parse(messageText);
        //console.log("message: ", parsedMessage);

        if (parsedMessage.id && parsedMessage.id in this.requests) {
          const { resolve, reject } = this.requests[parsedMessage.id]!;
          if (parsedMessage.error) {
            reject(new Error(JSON.stringify(parsedMessage.error)));
          } else {
            resolve(parsedMessage.result);
          }
          delete this.requests[parsedMessage.id];
        }
      };

      this.ws.onerror = (error) => {
        console.error("WebSocket error:", error);
        reject(error);
      };

      this.ws.onclose = (event) => {
        console.log("WebSocket connection closed:", event);
        // Clear any pending requests
        this.requests = {};
      };
    });
  }

  public static async new(url: string): Promise<Ogmios> {
    const instance = new Ogmios(url);
    await instance.setupEventListeners();
    return instance;
  }

  public static async fromDemeter(
    network: `mainnet` | `preview`,
    apiKey: `dmtr_ogmios${string}`,
    region: `ogmios-m1`,
  ): Promise<Ogmios> {
    const url = `wss://${apiKey}.${network}-v6.${region}.demeter.run`;
    return Ogmios.new(url);
  }

  private static generateId(): string {
    return crypto.randomUUID();
  }

  async request<
    T extends { params: object },
    R extends { result: any } | { error: object },
  >(method: string, params: T["params"]) {
    if (this.ws.readyState !== WebSocket.OPEN) {
      throw new Error("WebSocket connection not open");
    }
    const id = Ogmios.generateId();
    return new Promise<Extract<R, "result">>((resolve, reject) => {
      this.ws.send(
        JSON.stringify(
          { jsonrpc: "2.0", method, params, id },
          (_key, value) => {
            if (typeof value === "bigint") {
              return Number(value);
            }
            return value;
          },
        ),
      );
      this.requests[id] = { resolve, reject };
    });
  }

  public connect(): WebSocket {
    return this.ws;
  }

  public kill(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.ws) {
        this.ws.close();
      } else {
        // If there's no WebSocket, resolve immediately
        resolve();
      }
    });
  }

  // Rest of the methods...
  // ...
```
