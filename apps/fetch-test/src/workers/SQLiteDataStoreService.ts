import { ChainData, DataStoreService } from "./DataStoreService";
import sqlite3InitModule, { OpfsDatabase } from "@sqlite.org/sqlite-wasm";

import { DBNAME, SCHEMA, appendUpdateQuery, slotQuery } from "./schema";

export class SQLiteDataStoreService implements DataStoreService {
  db: OpfsDatabase;

  constructor() {
    // FIXME: It's VACUUMing
    this.init(); // cleanup()
  }

  async getLastSlot(): Promise<number> {
    const res = await this.query(slotQuery(), []);
    return res![0].last_slot as number;
  }

  // FIXME: Update SQLite database with new data
  async init(): Promise<any> {
    if (this.db) {
      return;
    }

    const sqlite3 = await sqlite3InitModule({
      print: console.log,
      printErr: console.error,
    });
    console.log(`Initializing SQL... ${sqlite3}`);

    try {
      // Testing in Node env
      if (typeof window === "undefined") {
        this.db = new sqlite3.oo1.DB(DBNAME, "c");
        console.log(`transient db = ${this.db.filename}`);
        await this.query(SCHEMA, []);
      } else if ("opfs" in sqlite3) {
        const opfsRoot = await navigator.storage.getDirectory();

        try {
          await opfsRoot.getFileHandle(DBNAME, { create: false });
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, "ct");
        } catch (e) {
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, "ct");

          console.log(
            "OPFS is available, created persisted database at",
            this.db.filename
          );

          await this.query(SCHEMA, []);
        }
      } else {
        console.error(`OPFS is not supported in the browser`);
      }
    } catch (err: any) {
      console.error(err.name, err.message);
    } finally {
      console.log("Initializing done...");
    }
  }

  async updateDatabase(data: ChainData): Promise<void> {
    if (!this.db) {
      return;
    }

    const tableName = data.tokenName[0];
    console.log(`@@@ Processing: ${data.tokenName}`);
    await this.query(
      appendUpdateQuery(tableName, data.tokenName, data.createdAt),
      []
    );

  }

  async query(sql: string, bind: any) {
    if (!this.db) {
      return;
    }
    const res = this.db.exec({
      sql,
      bind,
      returnValue: "resultRows",
      rowMode: "object",
    });
    return res;
  }

  async cleanup() {
    // Testing in Node env
    if (typeof window === "undefined") {
      this.query("VACUUM;", []);
    } else {
      const opfsRoot = await navigator.storage.getDirectory();
      await opfsRoot.removeEntry(DBNAME);
    }
    await this.init();
  }
}
