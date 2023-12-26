import { Assets, Data, Maestro, Translucent, addAssets, fromText, getAddressDetails, valueToAssets } from "translucent-cardano";
import * as constants from "../config/constants.ts";
import * as plutus from "../config/plutus.ts";
import * as keygen from "./generate-keys.ts";
import sha256 from "sha256";
import { StateHolderStateHolder } from "../config/plutus.ts";
import { sys } from "typescript";

const NW = constants.NETWORK;
const translucent = await Translucent.new(
  new Maestro({ network: NW, apiKey: Bun.env.MAESTRO_API_KEY! }), NW,);


// ----------------------------------------------------------------------------
// Show the dApp raw parameters to adjust.
// ----------------------------------------------------------------------------

constants.showConfig()

// ----------------------------------------------------------------------------
// Genereate the required collector credential.
// ----------------------------------------------------------------------------

/// Assuming collector address has some amount of money
const tr = await translucent.selectWalletFrom({ address: keygen.collectorAddress });
const utxos = await tr.wallet.getUtxos();

const utxo = utxos.find((value, inde, obj) => {
  return value.assets["lovelace"] / 1000000n > 50n;
})

const coll_utxo = utxos.find((value, inde, obj) => {
  return value.assets["lovelace"] / 1000000n == 5n;
})

console.log("\n######################### Credential details ##########################")
keygen.showDetails();
console.log(`UTxO                : ${utxo!.assets["lovelace"] / 1000000n}`);
console.log(`Collateral UTxO     : ${coll_utxo!.assets["lovelace"] / 1000000n}`);


// ----------------------------------------------------------------------------
// Initialise auth NFT token minting policy script
// ----------------------------------------------------------------------------

const authMintingPolicy = new plutus.OneshotAuthToken({
  transactionId: { hash: utxo!.txHash },
  outputIndex: BigInt(utxo!.outputIndex),
});

const authPolicyId = translucent.utils.mintingPolicyToId(
  authMintingPolicy,
);

console.log("\n##################### Auth token details ######################")
console.log(`Auth Token symbol   : ${authPolicyId}`);

// ----------------------------------------------------------------------------
// Initialise auth Always Fail validator as reference adress
// ----------------------------------------------------------------------------

const alwaysFailValidator = new plutus.AlwaysFailAlwaysFail();

const alwaysFailHash = translucent.utils.validatorToScriptHash(alwaysFailValidator);
const referenceAddress = translucent.utils.credentialToAddress(
  translucent.utils.scriptHashToCredential(alwaysFailHash),
);
console.log("\n##################### Reference script details ######################")
console.log(`Ref script address  : ${referenceAddress}`)

// ----------------------------------------------------------------------------
// Initialise StateHolder validator
// ----------------------------------------------------------------------------

const stateHolder = new plutus.StateHolderStateHolder(authPolicyId)


const stateHolderHash = translucent.utils.validatorToScriptHash(stateHolder);
const stateHolderCredential = translucent.utils.scriptHashToCredential(stateHolderHash);
const stateHolderAddress = translucent.utils.credentialToAddress(
  stateHolderCredential,
);

console.log("\n##################### State Holder validator details ######################")
console.log(`State holder hash   : ${stateHolderHash.toString()}`)
console.log(`State holder address: ${stateHolderAddress}`)

// ----------------------------------------------------------------------------
// Initialise Timelock deposit validator
// ----------------------------------------------------------------------------

const timeLock = new plutus.TimeDepositTimedeposit({
  collector: keygen.collectorPkh,
  collectionTime: BigInt(constants.COLLECTION_TIME)
});

const timeLockHash = translucent.utils.validatorToScriptHash(timeLock);
const timelockAddress = translucent.utils.credentialToAddress(
  translucent.utils.scriptHashToCredential(timeLockHash),
);
console.log("\n##################### TimeLock validator details ######################")
console.log("--------------------------------- Parameters --------------------------")
console.log(`collector's pk has  : ${keygen.collectorPkh}`)
console.log(`collection time     : ${new Date(constants.COLLECTION_TIME)}`)
console.log("-----------------------------------------------------------------------")
console.log(`Timelock script hash: ${timeLockHash.toString()}`)
console.log(`Timelock address    : ${timelockAddress}`)

// ----------------------------------------------------------------------------
// Initialise Adatag Minting Policy script
// ----------------------------------------------------------------------------

const mintingScript = new plutus.AdatagAdatagMinting(
  {
    authToken: authPolicyId,
    stateHolder: stateHolderHash.toString(),
    timedepositValidator: timeLockHash.toString(),
    deactivationTime: BigInt(constants.DEACTIVATION_TIME),
    depositBase: BigInt(constants.DEPOSIT_BASE),
    lockingDays: BigInt(constants.LOCKING_DAYS),
    adahandle: constants.ADAHANDLE,
  }
);

const mintingPolicyId = translucent.utils.mintingPolicyToId(mintingScript);
console.log("\n##################### Adatag Minting script details ######################")
console.log("--------------------------------- Parameters -----------------------------")
console.log(`auth token symbol   : ${keygen.collectorPkh}`)
console.log(`stateHolder hash    : ${stateHolderHash.toString()}`)
console.log(`timedeposit hash    : ${timeLockHash.toString()}`)
console.log(`deactivation time   : ${new Date(constants.DEACTIVATION_TIME)}`)
console.log(`deposit base        : ${constants.DEPOSIT_BASE}`)
console.log(`locking days        : ${constants.LOCKING_DAYS / 3600 / 1000 / 24}`)
console.log(`adahandle symbol    : ${constants.ADAHANDLE}`)


console.log("--------------------------------------------------------------------------")
console.log(`Minting hash        : ${mintingPolicyId}`)


// ----------------------------------------------------------------------------
// Genereate the required collector credential.
// ----------------------------------------------------------------------------
console.log("\n########################### Mock Adahandle ###########################")

const alwaysMintingScript = new plutus.AlwaysMintMint();
const alwaysMintPolicyId = translucent.utils.mintingPolicyToId(alwaysMintingScript);
console.log(`Always Minting hash : ${alwaysMintPolicyId}`)


// ----------------------------------------------------------------------------
// Build the deploying transaction
// ----------------------------------------------------------------------------

const empty_blake2b_224 = [0x83, 0x6c, 0xc6, 0x89, 0x31, 0xc2, 0xe4, 0xe3, 0xe8, 0x38, 0x60, 0x2e, 0xca, 0x19, 0x02, 0x59, 0x1d, 0x21, 0x68, 0x37, 0xba, 0xfd, 0xdf, 0xe6, 0xf0, 0xc8, 0xcb, 0x07,]
const empty_sha2_256 = [0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f, 0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,]

if (utxo && coll_utxo) {
  const tx = await translucent
    .newTx()
    .collectFrom(utxos)
    .mintAssets({
      [authPolicyId + fromText("a")]: 1n,
      [authPolicyId + fromText("b")]: 1n,
      [authPolicyId + fromText("c")]: 1n,
      [authPolicyId + fromText("d")]: 1n,
      [authPolicyId + fromText("e")]: 1n,
      [authPolicyId + fromText("f")]: 1n,
      [authPolicyId + fromText("g")]: 1n,
      [authPolicyId + fromText("h")]: 1n,
      [authPolicyId + fromText("i")]: 1n,
      [authPolicyId + fromText("j")]: 1n,
      [authPolicyId + fromText("k")]: 1n,
      [authPolicyId + fromText("l")]: 1n,
      [authPolicyId + fromText("m")]: 1n,
      [authPolicyId + fromText("n")]: 1n,
      [authPolicyId + fromText("o")]: 1n,
      [authPolicyId + fromText("p")]: 1n,
      [authPolicyId + fromText("q")]: 1n,
      [authPolicyId + fromText("r")]: 1n,
      [authPolicyId + fromText("s")]: 1n,
      [authPolicyId + fromText("t")]: 1n,
      [authPolicyId + fromText("u")]: 1n,
      [authPolicyId + fromText("v")]: 1n,
      [authPolicyId + fromText("w")]: 1n,
      [authPolicyId + fromText("x")]: 1n,
      [authPolicyId + fromText("y")]: 1n,
      [authPolicyId + fromText("z")]: 1n,
    }, Data.void())
    .payToContract(referenceAddress, { inline: Data.void(), scriptRef: timeLock, }, {})
    .payToContract(referenceAddress, { inline: Data.void(), scriptRef: stateHolder, }, {})
    .payToContract(referenceAddress, { inline: Data.void(), scriptRef: mintingScript, }, {})
    .attachMintingPolicy(authMintingPolicy)

  // from "a" to "z"  
  for (let i = 97; i <= 122; i++) {

    // FIXME: -- security issues? meaning  123 alma akorte == 123 alm aakorte
    // a: hash ("0" || "`" || "b" )
    // b: hash ("0" || "a" || "c" ) it should be c71325186e79cd85026361925fc7bae45dda0e166a832c1795ffa08e704801aa

    const val = [48, i - 1, i + 1]
    const hash_val = sha256(Array.from(val), { asBytes: true })

    const root = hash_val.concat(empty_sha2_256, empty_sha2_256)
    const root_hash = sha256(Array.from(root))

    const state: StateHolderStateHolder["oldState"] = {
      operationCount: 0n,
      operation: "AdatagRemoved",
      adatag: "",
      size: "0",
      rootHash: root_hash,
      mintingPolicy: mintingPolicyId
    }

    const id = Data.to(state, StateHolderStateHolder.oldState)
    tx.payToContract(stateHolderAddress, { inline: id, }, { [authPolicyId + i.toString(16)]: 1n, })
  }
  let txc = await tx.complete()

  console.log(txc.txComplete.to_json())
  let signedTx = txc.sign()
  console.log("######################### SIGNED #####################################")
  console.log(signedTx.txComplete.to_json())
  //const txHash = await signedTx.submit();
  //return txHash
}
else throw new Error("UTxO's Expected!")


