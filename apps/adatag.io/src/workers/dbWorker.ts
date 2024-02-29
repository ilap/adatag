import sqlite3InitModule from '@sqlite.org/sqlite-wasm';
import * as Comlink from 'comlink';
import { DBNAME, SCHEMA } from './constants';
import { QueryWorkerApi } from './types'

class QueryWorker implements QueryWorkerApi {
  
  db: any;
  initialising: boolean;

  constructor() {
    this.db = null;
    this.initialising = false;
  }

  async init(): Promise<any> {
    if (this.initialising || this.db) {
      return;
    }

    this.initialising = true;

    const sqlite3 = await sqlite3InitModule({
      print: console.log,
      printErr: console.error,
    });
    console.log('Initializing SQL...');

    try {
      if ('opfs' in sqlite3) {
        const opfsRoot = await navigator.storage.getDirectory();

        try {
          await opfsRoot.getFileHandle(DBNAME, { create: false });
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, 'ct');
        } catch (e) {
          this.db = new sqlite3.oo1.OpfsDb(DBNAME, 'ct');

          console.log(
            'OPFS is available, created persisted database at',
            this.db.filename,
          );

          await this.query(SCHEMA, []);
        }
      } else {
        console.error(`OPFS is not supported in the browser`);
      }
    } catch (err: any) {
      console.error(err.name, err.message);
    } finally {
      this.initialising = false;
      console.log('Initializing done...');
    }
  }

  async updateDatabase(transactions: any[]) {
    if (!this.db) {
      return;
    }

    // Begin transaction
    this.db.serialize(() => {
      this.db.run('BEGIN TRANSACTION');

      // Update database with new transactions
      transactions.forEach(transaction => {
        // Example: Insert transaction into a 'transactions' table
        this.db.run(
          'INSERT INTO transactions (id, amount, sender, recipient) VALUES (?, ?, ?, ?)',
          [
            transaction.id,
            transaction.amount,
            transaction.sender,
            transaction.recipient,
          ],
        );
      });

      // Commit transaction
      this.db.run('COMMIT');
    });

    // Close database connection
    this.db.close();
  }

  async query(sql: string, bind: any) {
    console.log(`Query...... ${this.db}`);
    if (!this.db) {
      return;
    }
    const res = await this.db.exec({
      sql,
      bind,
      returnValue: 'resultRows',
      rowMode: 'object',
    });
    console.log(`Query Result: ${res}`);
    return res;
  }

  select(table: string) {
    return this.query(`SELECT * from ${table}`, []);
  }

  async cleanup() {
    const opfsRoot = await navigator.storage.getDirectory();
    await opfsRoot.removeEntry(DBNAME);
    await this.init();
  }
}

const workerInstance = new QueryWorker();

Comlink.expose(workerInstance);
