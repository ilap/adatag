/*import { ChainData, DataStoreService } from './DataStoreService.ts'
import sqlite3InitModule, { OpfsDatabase, SqlValue } from '@sqlite.org/sqlite-wasm'


import { DBNAME, SCHEMA, appendUpdateQuery, slotQuery } from './schema.ts'
import { Val } from '../utils/types'

// DB query examples from https://sqlite.org/wasm/file/demo-123.js?txt
export class SQLiteDataStore implements DataStoreService {
  db: OpfsDatabase

  constructor() {
    // FIXME: It's VACUUMing
    this.init() // this.cleanup()
  }

  transformRow(row: SqlValue[]): Val {
    const xi = String(row[0])
    const xa = row[1] as string
    const xb = row[2] as string

    return { xi, xa, xb }
  }

  async getAllVal(tableName: string): Promise<[number, Val[]]> {
    return new Promise<[number, Val[]]>((resolve, reject) => {
      this.db.transaction('IMMEDIATE', async t => {
        try {
          const slot = this.db.selectValue(`SELECT MAX(tip) FROM config;`) as number
          const rows = this.db.selectArrays(`SELECT xi, xa, xb FROM ${tableName};`)
          const vals: Val[] = rows.map(row => this.transformRow(row))
          console.log(`#########  ${slot} \n ${JSON.stringify(vals)}`)
          resolve([slot, vals])
        } catch (error) {
          reject(error)
        }
      })
    })
  }

  async getLastSlot(): Promise<number> {
    return this.db.selectValue(slotQuery()) as number
  }

  async setLastSlot(slot: number): Promise<void> {
    await this.query(`UPDATE config SET tip = '${slot}' WHERE id = 1;`, [])
    return
  }

  // FIXME: Update SQLite database with new data
  async init(): Promise<any> {
    if (this.db) {
      return
    }

    const sqlite3 = await sqlite3InitModule({
      print: console.log,
      printErr: console.error,
    })
    console.log(`Initializing SQL... ${sqlite3}`)

    try {
      // Testing in Node env
      if (typeof window === 'undefined') {
        this.db = new sqlite3.oo1.DB(DBNAME, 'c')
        console.log(`transient db = ${this.db.filename}`)
        await this.query(SCHEMA, [])
      } else if ('opfs' in sqlite3) {
        const opfsRoot = await navigator.storage.getDirectory()

        try {
          await opfsRoot.getFileHandle(DBNAME, { create: false })
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, 'ct')
        } catch (e) {
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, 'ct')

          console.log('OPFS is available, created persisted database at', this.db.filename)

          await this.query(SCHEMA, [])
        }
      } else {
        console.error(`OPFS is not supported in the browser`)
      }
    } catch (err: any) {
      console.error(err.name, err.message)
    } finally {
      console.log('Initializing done...')
    }
  }

  async updateDatabase(data: ChainData): Promise<void> {
    if (!this.db) {
      return
    }

    const tableName = data.tokenName[0]
    console.log(`@@@ Processing: ${data.tokenName}`)
    await this.query(appendUpdateQuery(tableName, data.tokenName, data.createdAt), [])

    // XXXX:
    // console.log(`@@@ LLVALS: ${JSON.stringify(await this.getAllVal(tableName))}`)
  }

  async query(sql: string, bind: any) {
    if (!this.db) {
      return
    }
    const res = this.db.exec({
      sql,
      bind,
      returnValue: 'resultRows',
      rowMode: 'object',
    })
    return res
  }

  async cleanup() {
    // Testing in Node env
    if (typeof window === 'undefined') {
      this.query('VACUUM;', [])
    } else {
      const opfsRoot = await navigator.storage.getDirectory()
      await opfsRoot.removeEntry(DBNAME)
    }
    await this.init()
  }
}


*/
import { DataStoreService } from "./types";
import localforage from "localforage";

export class LocalForgeDataStore implements DataStoreService {
  private db: typeof localforage;

  constructor() {
    this.db = localforage.createInstance({
      name: "my-app-db",
      driver: localforage.INDEXEDDB, // This will use IndexedDB with a SQLite backend when available.
      storeName: "keyvaluepairs",
    });
  }

  async set(key: string, value: any): Promise<void> {
    await this.db.setItem(key, value);
  }

  async get(key: string): Promise<any> {
    return this.db.getItem(key);
  }

  async remove(key: string): Promise<void> {
    await this.db.removeItem(key);
  }

  async clear(): Promise<void> {
    await this.db.clear();
  }

  async length(): Promise<number> {
    return this.db.length();
  }

  async iterate(
    callback: (value: any, key: string, iterationNumber: number) => void,
  ): Promise<void> {
    await this.db.iterate(callback);
  }

  async key(index: number): Promise<string | null> {
    return this.db.key(index);
  }
}

