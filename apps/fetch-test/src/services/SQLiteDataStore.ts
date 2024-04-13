import sqlite3InitModule, { OpfsDatabase, SqlValue } from '@sqlite.org/sqlite-wasm'
import { Val, ChainData, DataStoreService } from './types.ts'
import { DBNAME, SCHEMA, appendUpdateQuery, slotQuery } from './schema.ts'
import { debugMessage } from '../utils.ts'

export class SQLiteDataStore implements DataStoreService {
  private static instance: SQLiteDataStore | null = null
  private db?: OpfsDatabase
  private initialized: boolean = false

  constructor() {}

  static getInstance(): DataStoreService {
    if (!SQLiteDataStore.instance) {
      SQLiteDataStore.instance = new SQLiteDataStore()
    }
    return SQLiteDataStore.instance
  }

  async initialise(): Promise<void> {
    try {
      const sqlite3 = await sqlite3InitModule({
        print: debugMessage,
        printErr: console.error,
      })
      debugMessage(`Initializing SQL... ${sqlite3}`)

      if ('opfs' in sqlite3) {
        const opfsRoot = await navigator.storage.getDirectory()
        try {
          await opfsRoot.getFileHandle(DBNAME, { create: false })
        } catch (e) {
          await this.createOpfsDb(sqlite3, opfsRoot)
        }
      } else if (typeof window === 'undefined') {
        this.db = new sqlite3.oo1.DB(DBNAME, 'c')
        debugMessage(`Transient DB created at: ${this.db.filename}`)
        await this.query(SCHEMA, [])
        debugMessage(`Initialisation SQL is done...`)
      }
      this.initialized = true
    } catch (err: any) {
      console.error('Initialization failed:', err)
      throw Error('SQLite Initialization failed')
    } finally {
      debugMessage('Initialization done...')
    }
  }

  private async createOpfsDb(sqlite3: any, opfsRoot: any): Promise<void> {
    this.db = new sqlite3.oo1.OpfsDb(DBNAME, 'ct')
    debugMessage(`OPFS is available, created persisted database at: ${this.db?.filename}`)
    await this.query(SCHEMA, [])
  }

  async cleanup(): Promise<void> {
    if (typeof window === 'undefined') {
      await this.query('VACUUM;', [])
    } else {
      const opfsRoot = await navigator.storage.getDirectory()
      await opfsRoot.removeEntry(DBNAME)
    }
    await this.initialise()
  }

  async getAllVal(tableName: string): Promise<[number, Val[]]> {
    return new Promise<[number, Val[]]>((resolve, reject) => {
      this.db?.transaction('IMMEDIATE', async t => {
        try {
          const slot = this.db?.selectValue(`SELECT MAX(tip) FROM config;`) as number
          const rows = this.db?.selectArrays(`SELECT xi, xa, xb FROM ${tableName};`)
          const vals: Val[] = rows!.map(row => this.transformRow(row))
          debugMessage(`#########  ${slot} \n ${JSON.stringify(vals)}`)
          resolve([slot, vals])
        } catch (error) {
          reject(error)
        }
      })
    })
  }

  private transformRow(row: SqlValue[]): Val {
    const [xi, xa, xb] = row.map(String)
    return { xi, xa, xb }
  }

  async getLastSlot(): Promise<number> {
    return this.db?.selectValue(slotQuery()) as number
  }

  async setLastSlot(slot: number): Promise<void> {
    await this.query(`UPDATE config SET tip = '${slot}' WHERE id = 1;`, [])
  }

  async query(sql: string, bind: any): Promise<any> {
    return this.db?.exec({
      sql,
      bind,
      returnValue: 'resultRows',
      rowMode: 'object',
    })
  }

  async updateDatabase(data: ChainData): Promise<void> {
    const tableName = data.tokenName[0]
    await this.query(appendUpdateQuery(tableName, data.tokenName, data.createdAt), [])
  }
}
