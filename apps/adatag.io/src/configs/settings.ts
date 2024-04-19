export const BOOTSTRAP_SLOT = 12
export const SAFETY_SLOTS = 100 // FIXME: 13 * 20 * 10
export const MS = 1000
// Buffer for time lock. e.g. for 20 days it's about an hour.
// Meaning the deposit is claimabile after around 20days and 1 hours.
export const TIMELOCK_BUFFER=1.002
export const KUPO_URL = 'http://localhost:1442'
export const OGMIOS_URL = 'ws://localhost:1337'

export const NETWORK = 'Custom'
export const ENV = 'Integration'

export const FETCH_DELAY = 7500
export const FETCH_TIMEOUT = 7500
export const DEBUG = true


// params for calculating the minimum timelock deposit. the mindeposit must be >= 5 (contract's settings)
export const MINDEPOSIT = 15
export const MAXDEPOSITLENGTH = 6
