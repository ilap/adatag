import * as Comlink from 'comlink'
import { Data, fromText, toText } from 'translucent-cardano'

import { KupmiosChainFetch } from '../services/KupmiosChainFetch.ts'
import { SQLiteDataStore } from '../services/SQLiteDataStore.ts'
import { ChainFetchService, DataStoreService, TreeState } from '../services/types.ts'
import { debugMessage, hexToASCII, stringifyData } from '../utils.ts'
import Config from '../configs/genesis-config.json'
import { IntegriTree } from '../../../../libs/integri-tree/src/lib/integri-tree.ts'
import * as T from '../configs/types.ts'
import * as P from '../configs/plutus.ts'
import { TreeWorkerService } from './types.ts'

class TreeWorker implements TreeWorkerService {
  constructor(
    private dataStore: DataStoreService,
    private chainFetch: ChainFetchService
  ) {}

  async initialise(): Promise<void> {
    // Initialise the database
    await this.dataStore.initialise()
    // Fetch the initial data
    await this.chainFetch.fetchAndSaveElements()
  }

  async createMintingDetails(adatag: string): Promise<{
    datum: string
    redeemer: string
  }> {
    await this.chainFetch.fetchAndSaveElements()
    const { tree, state } = await this.buildTreeFromChain(adatag)

    return await this.createTxInputs(adatag, tree, state)
  }

  private async buildTreeFromChain(
    adatag: string
  ): Promise<{ tree: IntegriTree; state: TreeState }> {
    try {
      // Authtoken is always the 1st char of the @adatag
      const authToken = adatag[0]

      // Retrieve the cached Vals and the slot of the last Val.
      const [cachedSlot, vals] = await this.dataStore.getAllVal(authToken)
      const tree = IntegriTree.fromList(vals)

      let fromSlot = cachedSlot
      let done = false
      let state = null

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

        state = Data.from(datum, P.StateHolderStateHolder.oldState) as TreeState

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
            `Inconsistent tree: ERROR & HALT ${state.rootHash} ${treeProof} ${elems[elems.length - 1]} .... ${state.adatag}`
          )
        }
        fromSlot = tip
      }

      if (!state) {
        throw new Error(`Could not serialise state.`)
      }

      return { tree, state }
    } catch (error) {
      console.error(`Error occurred while building tree: ${error}`)
      throw new Error(`Error occurred while building tree: ${error}`)
    }
  }

  private async createTxInputs(
    adatag: string,
    tree: IntegriTree,
    oldState: TreeState
  ): Promise<{ datum: string; redeemer: string }> {
    const minTree = tree.generateMinimalSubtree(adatag)

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
    const action: T.Operation = 'AdatagAdded'
    const size = (parseInt(toText(oldState.size)) + 1).toString()

    const newState: P.StateHolderStateHolder['oldState'] = {
      operationCount: oldState.operationCount + 1n,
      adatag: fromText(adatag),
      operation: action as T.Operation,
      size: fromText(size),
      rootHash: rootHash,
      mintingPolicy: Config.adatagMinting.policyId,
    }

    console.log(`Old State: ${stringifyData(oldState)}`)
    console.log(`New State: ${stringifyData(newState)}`)

    // Construct datum
    const datum = Data.to(newState, P.StateHolderStateHolder.oldState)

    // Construct redeemer
    const mintRedeemer: T.MintRedeemer = {
      Minting: [
        {
          updateVal: updateVal,
          appendVal: appendVal,
          proof: proof,
        },
      ], // Use 'as const' to assert that Minting is a tuple with a single element
    }

    const redeemer = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')

    return { datum, redeemer }
  }
}

let _workerInstance: TreeWorkerService | null = null

// Use a factory function to create the worker with dependencies
export async function createWorker(): Promise<TreeWorkerService> {
  if (!_workerInstance) {
    try {
      const dataStore = new SQLiteDataStore()
      const fetch = new KupmiosChainFetch(dataStore)
      _workerInstance = new TreeWorker(dataStore, fetch)
      await _workerInstance.initialise()
    } catch (error) {
      throw new Error(`Error initializing translucent has failed: ${error}`)
    }
  }
  return _workerInstance
}

const instance = await createWorker()
// Expose the worker
Comlink.expose(instance)
