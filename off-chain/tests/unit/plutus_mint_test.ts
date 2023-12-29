import {
  applyParamsToScript,
  Blockfrost,
  Data,
  fromText,
  Translucent,
  MintingPolicy,
  PolicyId,
  TxHash,
  Unit,
  toUnit,
} from "translucent-cardano";

import { setupTestInstance } from "../utils.ts";
import { OneshotAuthToken } from "../../config/plutus.ts";
/*
// Always succeed plutus minting policy script 
const mintingPolicy: MintingPolicy = {
  type: "PlutusV2",
  script: "51010000322253330034a229309b2b2b9a01",
};
// It should be d8906ca5c7ba124a0407a32dab37b2c82b13b3dcd9111e42940dcea4
const policyId = (await Translucent.new(undefined, "Custom")).utils.mintingPolicyToId(mintingPolicy);
*/


function getMintingPolicy(utxoRef: { transactionId: { hash: string }; outputIndex: bigint }): MintingPolicy {
  return {
    type: "PlutusV2",
    script: applyParamsToScript(
      "5901d501000032323232323232323222225333007323232323232323232323253330123370e900018088040991919299980a99b8748000c0500044c8cdd79ba73232323300100100222533301d00114bd7009980f180d980f8009980100118100009919299980c99b874800800452f5bded8c026eacc078c05c008c05c004c8cc004004008894ccc070004530103d87a8000132323232533301d3371e014004266e95200033021374c00297ae01330060060033756603c0066eb8c070008c080008c078004c8cc004004018894ccc06c00452f5bded8c0264646464a66603866e3d221000021003133020337606ea4008dd3000998030030019bab301d003375c6036004603e004603a0029801369f416141624163416441654166416741684169416a416b416c416d416e416f4170417141724173417441754176417741784179417aff00301300116323300100100922533301900114c103d87a80001323253330183375e603a602c00402a266e9520003301c0024bd70099802002000980e801180d8009bae30180013010008163756602c002602c002602a002602800260260046eb0c044004c02400cc03c004c03c008c034004c01400452613656230053754002460066ea80055cd2ab9d5573caae7d5d02ba15745",
      [utxoRef],
      {
        "dataType": "list",
        "items": [{
          "title": "OutputReference",
          "description":
            "An `OutputReference` is a unique reference to an output on-chain. The `output_index`\n corresponds to the position in the output list of the transaction (identified by its id)\n that produced that output",
          "anyOf": [{
            "title": "OutputReference",
            "dataType": "constructor",
            "index": 0,
            "fields": [{
              "title": "transactionId",
              "description":
                "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
              "description2":
                "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
              "anyOf": [{
                "title": "TransactionId",
                "dataType": "constructor",
                "index": 0,
                "fields": [{ "dataType": "bytes", "title": "hash" }],
              }],
            }, { "dataType": "integer", "title": "outputIndex" }],
          }],
        }],
      } as any,
    ),
  };
}

async function getPolicyId(mp: MintingPolicy): Promise<string> {
  return (await Translucent.new(undefined, "Custom")).utils.mintingPolicyToId(mp);
}

it("Plutus Mint(success): One shot minting", async () => {
  const { translucent, address, details } = await setupTestInstance({ accountIndex: 0 });

  const [utxo] = await translucent.wallet.getUtxos();
  const txh = utxo.txHash
  const idx = utxo.outputIndex

  //const mintingPolicy = getMintingPolicy({ transactionId: { hash: txh }, outputIndex: BigInt(idx) })
  const mintingPolicy = new OneshotAuthToken({
    transactionId: { hash: txh },
    outputIndex: BigInt(idx)
  })

  const policyId = await getPolicyId(mintingPolicy)
  console.log(`policy id: ${await policyId}`)

  const tx = await translucent
    .newTx()
    .mintAssets({
      [policyId + fromText("a")]: 1n,
      [policyId + fromText("b")]: 1n,
      [policyId + fromText("c")]: 1n,
      [policyId + fromText("d")]: 1n,
      [policyId + fromText("e")]: 1n,
      [policyId + fromText("f")]: 1n,
      [policyId + fromText("g")]: 1n,
      [policyId + fromText("h")]: 1n,
      [policyId + fromText("i")]: 1n,
      [policyId + fromText("j")]: 1n,
      [policyId + fromText("k")]: 1n,
      [policyId + fromText("l")]: 1n,
      [policyId + fromText("m")]: 1n,
      [policyId + fromText("n")]: 1n,
      [policyId + fromText("o")]: 1n,
      [policyId + fromText("p")]: 1n,
      [policyId + fromText("q")]: 1n,
      [policyId + fromText("r")]: 1n,
      [policyId + fromText("s")]: 1n,
      [policyId + fromText("t")]: 1n,
      [policyId + fromText("u")]: 1n,
      [policyId + fromText("v")]: 1n,
      [policyId + fromText("w")]: 1n,
      [policyId + fromText("x")]: 1n,
      [policyId + fromText("y")]: 1n,
      [policyId + fromText("z")]: 1n,
    },
      Data.void())
    .attachMintingPolicy(mintingPolicy)
    .complete();

  const signedTx = await tx.sign().complete();
  console.log(signedTx.txSigned.to_json())

  await expect(signedTx.submit()).resolves.toBeDefined();
});
