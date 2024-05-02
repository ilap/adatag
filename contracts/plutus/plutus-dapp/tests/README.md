# Unit Tests

> **Note**: Developers are responsible for ensuring the proper bootstrap of the @adatag system.

All unit tests assume a valid bootstrap, including:
1. The system has valid authorization Tokens (`cnftpid."a"` through `cnftpid."z"`).
2. The StateHolder uses the relevant authorization Token's currency symbol as a parameter for generating its compiled code.
3. Each authorization Token is sent to a state-holder address individually, each with a valid initial state (inline datum):
    - Operation count: `0`
    - Adatag: `""`
    - Tree state: `Initial`
    - Tree size: `0`
    - Proof: The hash of the relevant empty tree, using `(cnft token name -1, cnft token name +)` as constraints. For example, if "cnftpid"."a," then ```H( H("`","b") || H("") | H(""))```
    - Minting policy: The relevant minting policy id.
4. Proper setup of the TimeLockDeposit validator using correct parameters, including:
    - Collector's (developer's) public key hash.
    - Collection time (twice the minting policy's deactivation time, calculated at bootstrap time).
5. Proper setup of the MintingPolicy minting script with correct parameters, including:
    - Correct authorization Token currency symbol.
    - Correct StateHolder validator policy id.
    - Correct TimeLockDeposit vaildator's policy id.
    - Correct lock feature expiry time, usually around 6 months from bootstrap.
    - Correct user locking period, typically 20 days from deposit.
    - Maximum deposit that users must lock to prevent abusing the system initially.
6. Sending all three scripts to the AlwaysFailValidator's address as reference scripts (ensuring the existence of 3 EUTxOs).

It's important to note that other settings such as datums, redeemers, and built transactions are created by the user's frontend/off-chain codes over which the system has no control.