import * as Comlink from 'comlink'
import { Data, fromText, toText } from 'translucent-cardano'

import { KupmiosChainFetch } from '../services/KupmiosChainFetch'
import { SQLiteDataStore } from '../services/SQLiteDataStore'
import { ChainFetchService, DataStoreService, TreeState } from '../services/types'
import { debugMessage, hexToASCII, stringifyData } from '../utils'

import { IntegriTree } from '@adatag/integri-tree'

import {
  TimeDepositDatum,
  StateHolderStateHolder,
  MintRedeemer,
  Operation,
  AdatagAdatagMinting,
  Val,
} from '@adatag/common/plutus'
import { TreeWorkerService } from './types'
import { genesisConfig } from '../utils/config'

class TreeWorker implements TreeWorkerService {
  constructor(private dataStore: DataStoreService, private chainFetch: ChainFetchService) {}

  async initialise(): Promise<void> {
    // Initialise the database
    await this.dataStore.initialise()
    // Fetch the initial data
    await this.chainFetch.fetchAndSaveElements()
  }

  async checkIfAdatagMinted(adatag: string): Promise<boolean> {
    //const a = await this.getDepositDetails(adatag)
    //debugMessage(`@@@@@###### DATUM ${JSON.stringify(a)}`)
    return (await this.chainFetch.fetchAsset(adatag)) !== undefined
  }

  async checkIfAdatagNotMinted(adatag: string): Promise<boolean> {
    return (await this.getDepositDetails(adatag)) === undefined
  }

  async getDepositDetails(adatag: string): Promise<
    | {
        txId: string
        outputIndex: number
        beneficiary: string
        deadline: number
      }
    | undefined
  > {
    // 1. Retrieve the oldest transaction output for the given adatag
    const asset = await this.chainFetch.fetchAsset(adatag, true)

    if (!asset) {
      return undefined
    }

    // 2. Retrieve the datum for the transaction output
    const result = await this.chainFetch.fetchDatum(asset.transaction_id, genesisConfig!.timelockScript.scriptAddress)

    if (!result) {
      return undefined
    }

    //const datum = Data.from(result.datum, P.TimeDepositTimedeposit.datum as  unknown as TreeState)
    const datum: TimeDepositDatum['datum'] = Data.from(result.datum, TimeDepositDatum.datum)
    debugMessage(`@@@@@@@ ${stringifyData(datum)} ... from ${result.datum}`)
    // 3. Return the transaction ID, output index, and datum
    return {
      txId: asset.transaction_id,
      outputIndex: result.output_index,
      beneficiary: datum.beneficiary,
      deadline: Number(datum.deadLine),
    }
  }

  serialiseVal(val: Val): Val {
    return {
      xi: fromText(val.xi),
      xa: fromText(val.xa),
      xb: fromText(val.xb),
    }
  }

  async createMintingDetails(adatag: string): Promise<{
    datum: string
    redeemer: string
  }> {
    debugMessage(`###### 1. Fetching Chain `)
    await this.chainFetch.fetchAndSaveElements()
    debugMessage(`###### 2. Building Tree `)
    const { tree, state } = await this.buildTreeFromChain(adatag)

    debugMessage(`###### 3. Constructing Datum and Redeemer`)
    return await this.createTxInputs(adatag, tree, state)
  }

  private async buildTreeFromChain(adatag: string): Promise<{ tree: IntegriTree; state: TreeState }> {
    // try {
    // Authtoken is always the 1st char of the @adatag
    const authToken = adatag[0]

    // Retrieve the cached Vals and the slot of the last Val.
    const [cachedSlot, vals] = await this.dataStore.getAllVal(authToken)
    const tree = IntegriTree.fromList(vals)

    let fromSlot = cachedSlot
    let done = false
    let state: TreeState | undefined = undefined

    while (!done) {
      // Fetch all missing elements till the tip
      const tip = await this.chainFetch.fetchTip()
      const elems = await this.chainFetch.fetchElements(fromSlot, tip)

      const filteredElems = elems.filter((elem: string) => elem[0] === authToken)

      filteredElems.forEach(elem => {
        debugMessage(`Append elem: ${elem}`)
        tree.append(elem)
      })

      // Retrieve the slot and the stored state from datum as CBOR from the eUTxO of the auth token.
      const fetchedDatum = await this.chainFetch.fetchStateDatum(authToken)

      if (!fetchedDatum) {
        throw Error(`Could not fetch datum for auth token "${authToken}".`)
      }

      const { datum } = fetchedDatum

      state = (Data.from(datum, StateHolderStateHolder.oldState) as TreeState) || undefined

      if (!state) {
        throw Error(`Cannot convert datum to typescript's type`)
      }
      // Sanity checks
      const treeProof = tree.rootHash()

      const validTree =
        state.rootHash === treeProof && filteredElems.length > 0
          ? hexToASCII(state.adatag) === filteredElems[filteredElems.length - 1]
          : true

      if (validTree) {
        debugMessage(`The tree is consistent with state in the datum`)
        done = true
      } else {
        throw new Error(
          `Inconsistent tree: ERROR & HALT ${state.rootHash} ${treeProof} ${elems[elems.length - 1]} .... ${
            state.adatag
          }`
        )
      }
      fromSlot = tip
    }

    if (!state) {
      throw new Error(`Could not serialise state.`)
    }

    return { tree, state }
    //} catch (error) {
    //  console.error(`Error occurred while building tree: ${error}`)
    //  throw new Error(`Error occurred while building tree: ${error}`)
    //}
  }

  private async createTxInputs(
    adatag: string,
    tree: IntegriTree,
    oldState: TreeState
  ): Promise<{ datum: string; redeemer: string }> {
    const minTree = tree.generateMinimalSubtree(adatag, this.serialiseVal)

    if (!minTree) {
      throw new Error(`Cannot create minimal subtree for "${adatag}". Probably minted already.`)
    }

    const { updateVal, appendVal, proof } = minTree

    tree.append(adatag)

    // generate the root hash of the appended tree.
    const rootHash = tree.rootHash()

    debugMessage(`Update VAL: ${JSON.stringify(updateVal)}`)
    debugMessage(`Append VAL: ${JSON.stringify(appendVal)}`)
    debugMessage(`Proof     : ${JSON.stringify(proof)}`)
    debugMessage(`New Root  : ${JSON.stringify(rootHash)}`)
    debugMessage(`Old root  : ${JSON.stringify(oldState.rootHash)}  `)

    // Construct the new tree state
    const action: Operation = 'AdatagAdded'
    const size = (parseInt(toText(oldState.size)) + 1).toString()

    const newState: StateHolderStateHolder['oldState'] = {
      operationCount: oldState.operationCount + 1n,
      adatag: fromText(adatag),
      operation: action as Operation,
      size: fromText(size),
      rootHash: rootHash,
      mintingPolicy: genesisConfig!.adatagMinting.policyId,
    }

    debugMessage(`Old State: ${stringifyData(oldState)}`)
    debugMessage(`New State: ${stringifyData(newState)}`)

    // Construct datum
    const datum = Data.to(newState, StateHolderStateHolder.oldState)

    // Construct redeemer
    const mintRedeemer: MintRedeemer = {
      Minting: [
        {
          updateVal: updateVal,
          appendVal: appendVal,
          proof: proof,
        },
      ], // Use 'as const' to assert that Minting is a tuple with a single element
    }

    const redeemer = Data.to(mintRedeemer, AdatagAdatagMinting.rdmr, 'proof')

    return { datum, redeemer }
  }
}

let _workerInstance: TreeWorkerService | null = null

// Use a factory function to create the worker with dependencies
export async function createWorker(): Promise<TreeWorkerService> {
  if (!_workerInstance) {
    try {
      const dataStore = SQLiteDataStore.getInstance()
      const fetch = new KupmiosChainFetch(dataStore)
      // FIXME: comment out after tests...
      // await dataStore.cleanup()
      _workerInstance = new TreeWorker(dataStore, fetch)
      await _workerInstance.initialise()
      debugMessage(`TreeWorker is initialised.`)
    } catch (error) {
      throw new Error(`Error initializing tree worker: ${error}`)
    }
  }
  return _workerInstance
}

const instance = await createWorker()
Comlink.expose(instance)
