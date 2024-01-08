// Curr time range is in some interval e.g. {5000, 7000}, booth inclusive
// Rule: the deadline/collection time must be lower or equal with lower bound of curr time range

// These below is for checkint user created outputs
// pub const base_deposit = 1750
// pub const day = 86400000
// pub const locking_days = 1717200000 // 20 * 86400000 - 3 * 3600000 -> 1728000000 - 10800000

// These below is for collecting or 
// Replace it with some valid date after bootstrap.
// Preferably half year for feature active and 1yr for collecting
// unclaimed deposits donations.
// FIXME: replace  all these below with the valid valued
// The bootstrap day/time in milliseconds
//pub const current_time = 0
// Preferably 6+12 months after current_time
//pub const collect_after = 31536000000 // current_time + 31536000000,  365 * 86400000
// Preferably tha half ot the collect_after time.
//pub const feature_expired = 86400000 // 15811200000 // current_time + collect_after / 2 , ~ 183 * 86400000

// FIXME:
// The POSIXTime (in ms) until the Lock Time Deposit Feature is active
// (e.g. ~6-9 months form the @adatag bootstrapping).
// The users deposit locked in days, if the feature is active. Preferably 20 days.
// The user's `deadline` in the datum - calculated by the front-end is currentTime + userDepositLockingDays * dayInseconds
// Note: the script would fail if the user's lock end date time in the output datum is larger than the currentTime + 2 * userDepositLockingDays * 86400
// to prevent any accidental over locking.
// The user's min deposit for locking an @adatag.  locking_day: Int,
//timelock_expiry: PosixTime,
// The deposint base from which the user's deposit is calculated.
// deposit_base: Int,
