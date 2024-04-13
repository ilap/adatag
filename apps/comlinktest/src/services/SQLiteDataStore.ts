import sqlite3InitModule, { OpfsDatabase, SqlValue } from '@sqlite.org/sqlite-wasm'

import { Val, ChainData, DataStoreService } from './types.ts'
import { DBNAME, SCHEMA, appendUpdateQuery, slotQuery } from './schema.ts'

// DB query examples from https://sqlite.org/wasm/file/demo-123.js?txt
export class SQLiteDataStore implements DataStoreService {
  private static instance: SQLiteDataStore | null = null;
  private db?: OpfsDatabase;
  private initialized: boolean = false;

  private constructor() {
    if (!this.initialized) {
      this.init().then(() => {
        this.initialized = true;
      }).catch(err => {
        console.error("Initialization failed:", err);
        throw Error('SQLite Initialisation is failed')
      });
    }
  }

  static getInstance(): SQLiteDataStore {
    if (!SQLiteDataStore.instance) {
      SQLiteDataStore.instance = new SQLiteDataStore();
    }
    return SQLiteDataStore.instance;
  }

  private transformRow(row: SqlValue[]): Val {
    const xi = String(row[0])
    const xa = row[1] as string
    const xb = row[2] as string

    return { xi, xa, xb }
  }

  // FIXME: Update SQLite database with new data
  async init(): Promise<any> {

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


  async cleanup(): Promise<void> {
    // Testing in Node env
    if (typeof window === 'undefined') {
      this.query('VACUUM;', [])
    } else {
      const opfsRoot = await navigator.storage.getDirectory()
      await opfsRoot.removeEntry(DBNAME)
    }
    await this.init()
  }

  async getAllVal(tableName: string): Promise<[number, Val[]]> {
    return new Promise<[number, Val[]]>((resolve, reject) => {
      this.db?.transaction('IMMEDIATE', async t => {
        try {
          const slot = this.db?.selectValue(`SELECT MAX(tip) FROM config;`) as number
          const rows = this.db?.selectArrays(`SELECT xi, xa, xb FROM ${tableName};`)
          const vals: Val[] = rows!.map(row => this.transformRow(row))
          console.log(`#########  ${slot} \n ${JSON.stringify(vals)}`)
          resolve([slot, vals])
        } catch (error) {
          reject(error)
        }
      })
    })
  }

  async getLastSlot(): Promise<number> {
    return this.db?.selectValue(slotQuery()) as number
  }

  async setLastSlot(slot: number): Promise<void> {
    await this.query(`UPDATE config SET tip = '${slot}' WHERE id = 1;`, [])
  }

  async query(sql: string, bind: any) {
    return this.db?.exec({
      sql,
      bind,
      returnValue: 'resultRows',
      rowMode: 'object',
    })
  }

  async updateDatabase(data: ChainData): Promise<void> {
    const tableName = data.tokenName[0]
    //console.log(`@@@ Processing: ${data.tokenName}`)
    await this.query(appendUpdateQuery(tableName, data.tokenName, data.createdAt), [])
  }
}
