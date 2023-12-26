// Bootstrap time in UNIX epoch
export const HASH_ALG ="SHA2_256"
export const NETWORK = "Preview"
export const BOOTSTRAP_TIME = Date.now()
export const DEPOSIT_BASE = 1750
export const SECRETS_DIR = "./secrets/"
export const COLLECTOR_KEY = "collector.sk"
export const COLLECTOR_SEED = "collector.seed"
export const COLLECTOR_ADDR = "collector.addr"
export const COLLECTION_TIME = BOOTSTRAP_TIME + days(2)
export const LOCKING_DAYS = days(20)
export const DEACTIVATION_TIME = BOOTSTRAP_TIME + days(1)
// Only exists on mainnet, for development 
// create some very simple NFT minting policy that everybody can mint burn 
export const ADAHANDLE = "010203040506070809"

function days(n: number) {
    return 3600 * 1000 * 24 * n
}

function showConfig() {
    console.log("####################### dApp Parameters are ##########################")
    console.log(`Network          : ${NETWORK}`)
    console.log("!!!!!!!!!!!!!!!!!!!!!!!!!!! IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    console.log(`Hash algorithm   : ${HASH_ALG}`)
    console.log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    console.log(`Bootstrap time   : ${new Date(BOOTSTRAP_TIME)}`)
    console.log(`Deactivation time: ${new Date(DEACTIVATION_TIME)}`)
    console.log(`Collection time  : ${new Date(COLLECTION_TIME)}`)

    console.log(`Locking days     : ${LOCKING_DAYS / 3600 / 1000 / 24}`)
    console.log(`Deposit base     : ${DEPOSIT_BASE}`)

    console.log(`Collector key    : ${COLLECTOR_KEY}`)
    console.log(`Collector addr   : ${COLLECTOR_ADDR}`)
    console.log(`Adahandle        : ${ADAHANDLE}`)
}

export { showConfig }

// showConfig()