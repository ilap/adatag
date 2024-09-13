import sqlite3InitModule, { OpfsDatabase, SqlValue, BindingSpec } from '@sqlite.org/sqlite-wasm'
import { Val, ChainData, DataStoreService } from './types'
import { DBNAME, SCHEMA, appendUpdateQuery, slotQuery } from './schema'
import { DEBUG } from '../configs/settings'

//const DEBUG = null

export class SQLiteDataStore implements DataStoreService {
  private static instance: SQLiteDataStore | null = null
  private db?: OpfsDatabase

  private constructor() {}

  public static getInstance(): DataStoreService {
    if (!SQLiteDataStore.instance) {
      SQLiteDataStore.instance = new SQLiteDataStore()
    }
    return SQLiteDataStore.instance
  }

  public async initialise(): Promise<void> {
    try {
      console.log(`Initializing SQLite module.`)
      const sqlite3 = await sqlite3InitModule({
        print: console.log,
        printErr: console.error,
      })

      if ('opfs' in sqlite3) {
        try {
          const opfsRoot = await navigator.storage.getDirectory()
          await opfsRoot.getFileHandle(DBNAME, { create: false })
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, DEBUG ? 'ct' : 'c')
          console.log(`OPFS is available, existing persisted database at: ${this.db?.filename}`)
        } catch (e) {
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, DEBUG ? 'ct' : 'c')
          await this.query(SCHEMA, [])
          console.log(`OPFS is available, created persisted database at: ${this.db?.filename}`)
        }
      } else if (typeof window === 'undefined') {
        this.db = new sqlite3.oo1.DB(DBNAME, DEBUG ? 'ct' : 'c')
        console.log(`Transient DB created at: ${this.db.filename}`)
        await this.query(SCHEMA, [])
        console.log(`Initialization SQL is done.`)
      }
    } catch (err) {
      console.log(`Initialization failed: ${err}`)
      throw Error('SQLite Initialization failed')
    } finally {
      console.log('Initialization done...')
    }
  }

  public async cleanup(): Promise<void> {
    try {
      console.log(`Removing ${DBNAME}`)
      console.log(`Removing ${DBNAME}`)
      this.db?.close()
      const opfsRoot = await navigator.storage.getDirectory()
      await opfsRoot.removeEntry(DBNAME, { recursive: true })
    } catch (e) {
      console.log(`CLEANING: ${e}`)
    } finally {
      console.log(`DB Cleaning is done`)
    }
  }

  public async getAllVal(tableName: string): Promise<[number, Val[]]> {
    return new Promise<[number, Val[]]>((resolve, reject) => {
      this.db?.transaction('IMMEDIATE', async () => {
        try {
          const slot = this.db?.selectValue(`SELECT MAX(tip) FROM config;`) as number
          const rows = this.db?.selectArrays(`SELECT xi, xa, xb FROM ${tableName};`)
          const vals: Val[] = rows!.map(this.transformRow)
          //console.log(`#########  ${slot} \n ${JSON.stringify(vals)}`);
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

  public async getLastSlot(): Promise<number> {
    return this.db?.selectValue(slotQuery()) as number
  }

  public async setLastSlot(slot: number): Promise<void> {
    await this.query(`UPDATE config SET tip = '${slot}' WHERE id = 1;`, [])
  }

  public async query(sql: string, bind?: BindingSpec): Promise<{ [columnName: string]: SqlValue }[] | undefined> {
    return this.db?.exec({
      sql,
      bind,
      returnValue: 'resultRows',
      rowMode: 'object',
    })
  }

  public async updateDatabase(data: ChainData): Promise<void> {
    const tableName = data.tokenName[0]
    await this.query(appendUpdateQuery(tableName, data.tokenName, data.createdAt), [])
  }
}
