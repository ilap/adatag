
## Introduction

The @adatag dApp project implements a simple but powerful NFT-based username
management system that consists the following core components:

- [On-chain logic](#on-chain-logic)
  - [Control NFT Minting Policy](#control-nft-minting-policy)
  - [StateHolder Validator](#control-nft-validator)
  - [Time Deposit Validator](#time-deposit-validator)
  - [Adatag Minting Policy](#adatag-minting-policy)
- [Front-en (off-chain) logic](#front-end-off-chain-logic)

## On-chain logic

Assumptions:

- A datum must be attached to an UTXO for a script address.
- The maximum size of an inline datum in an EUTxO is 4096 bytes (4KB)
- A transaction only passes when all of the script validators of the transaction
  pass.
- If there is a MintInfo in a transaction, the relevant minting script must be
  presented in the transaction either as a reference script or as an attached
  script to the transaction.
- If there is an output of a token, then there must be at least one
  non-reference input in the transaction.
- More than one validator script can run in the same transaction.
- Only one minting policy script can run in the same transaction for minting/burning any tokens for the same currency symbol.
- The datums and redeemers are stored in the transaction's scriptDatahash field.
- The scripts are stored in the witness set or at the reference script address
  as the hash of the script/validator.
- The order in which the scripts in a transaction are run is based on the order
  of their policy IDs (hashes of the policy IDs).
- Phase 1 validation fails when there is a reference input and a non-reference
  input in the same transaction.
- If there is a MintInfo in a transaction with a token same with a minting
  policy Id attached to the transaction, the minting policy must be run.

### Control NFT Minting Policy

This is a one-time NFT creation policy used to make control NFTs (with token
names: `"a",...,"z"`). These NFTs are used by the StateHolder and Minting Policy Plutus scripts. They hold the state (in their datum) of the associated adatag
Tree (`Ta,...,Tz`), which holds the relevant public usernames. For example, the
proof of the adatag tree `Ta`, carried by the control NFT has token name `'a'`,
only includes usernames starting with `'a'`, and so forth.

### StateHolder Validator

At bootstrap, every control NFT, along with the datum containing the initial
state of the relevant adatag tree, is sent to the StateHolder validator address.
This StateHolder’s only responsibility is to transfer the control NFT from the
StateHolder’s address (which holds the old state) to the same address (now
containing the newly updated state by the minting script).

> Note: **Importantly**, the StateHolder validator **does not** modify the tree's state; it
> only stores the updated (by the minting script) state in the datum of its
> address UTxO together with the control NFT token.


#### Validation logic

The validation logic for handling the transfer of the relevant tree state must
enforce that no adversarial attempt could invalidate (i.e. alter) the new state
by allowing somehow to spend (destroy) the UTxO holding the old correct state
and creating a new UTxO for the same address with the control NFT and invalid
state (e.g. spending it without running the adatag minting script in the
transaction).

It is important to ensure that the adatag minting script is executed for the
same transaction as the state-holder and the optional time-lock deposti validators. 
This is because it ensures that the new state
of the adatag tree is validated before it is committed to the blockchain. If the
minting script is not executed for the same transaction, then an attacker could
potentially create a new UTxO for the control NFT with an invalid state, and the
StateHolder would not be able to detect it.

The StateHolder  validator checks that the adatag retrieved from the output UTxO exists in
the MintInfo's Value as the token name of the token of which currency symbol
(policy ID) is the adatag minting policy id. This check ensures that the adatag
being minted or burned is the same adatag that is associated with the control
NFT being transferred. This ensure that the adatag minting policy will be
executed in the same transaction.

#### Validation rules

To achieve the above the following StateHolder checks need to be enforced:

1. Check that its current own input (the spending UTxO that is being validated)
   contains only one control NFT token.
2. Check that the same and only token is in an output that contains the
   StateHolder address.
   - **IMPORTANT:** as a control NFT input in the rule 1. can be a reference
     input too (the UTxO is not being spent but only referenced).
3. Check that the adatag retrieved from the Output UTxO exists in the MintInfo's
   Value as the token name of the token of which currency symbol's (policy ID)
   is the adatag minting policy id.

> A reference input is a UTxO that is referenced in a transaction but is not
> being spent. This can be useful for cases where the data associated with the
> UTxO is needed by the transaction, but the UTxO itself does not need to be
> spent.

These above validates that:

- **Only one control NFT is being spent in its current execution of the
  StateHolder script.** This is because there is only one output that contains
  only the relevant control NFT token.
- **It has a well-formed new state (a valid datum at the StateHolder's script
  address).** This is because the adatag being burned or minted is the same
  adatag that is associated with the control NFT being transferred.
- **Proof of the presence and execution of the adatag minting script in the
  current transaction.** This is because the token name of the minting/burning
  adatag is found in the MintInfo's Value and the reference input of the adatag
  minting policy is in the inputs of the current transaction.

### Time Deposit Validator

When creating a public `@adatag` (username), an automatically enforced
time-locked deposit helps prevent potential abuse within the system (bulk buying
adatags for selling them later).

However, this locked deposit is not mandatory if the new @adatag username
perfectly matches the user's $adahandle, and the $adahandle is added to the
transaction's input and output (minting policy handles this).

After the predefined deadline (typically around 20 days), users can claim their
deposit.

The time-locked deposit feature will be deactivated after a certain period
(usually 6-9 months) from starting the @adatag.

Additionally, the developers (currently only ilap) reserve the right to collect
any unclaimed time deposits around after twice the deactivation date of the
time-locked deposit feature (set aat bootstrap time).

For example, if the deactivation time is 6 months after the system bootstrap,
they may collect unclaimed deposits around after 12 months from the bootstrap.

The time deposit can also be used for donation to the developers, as a way to
appreciate their hard work and effort in creating and maintaining the @adatag.

> Note: Any collected deposit by the developers will certainly be paid back to
> the rightful owner. To prove rightful ownership of an unclaimed but collected
> deposit, the owner must present a signature created by the private key of the
> public key specified in the timed locked deposit as the beneficiary.
> Alternatively, a transaction from the beneficiary address (included in the
> collected claim) to the same address containing a developer-specified small
> amount of ADA (e.g., 1.234567 ADA) can also serve as proof of rightful
> ownership.

#### Validation logic

To achieve the above, the following TimeDeposit checks need to be enforced:

- **Beneficiary claim:**
  - Checks whether the deadline has passed.
  - Checks that the beneficiary has a valid signature (public key hash in the
    datum).
- **Developer collect:**
  - Checks whether collect deadline time has passed.
  - Checks that the developer has a valid signature (public key hash as
    parameter).
- **Developer donation:**
  - Checks that the input UTxO has a void/unit datum.
  - Checks that the script address has a valid signature (the same public key
    hash is known to the script).

### Adatag Minting Policy

The Adatag minting policy is a complex validator that ensures the integrity of
the Adatag system. It verifies that time-locked deposit outputs, minting/burning
logic, and the old and new states of the Adatag tree are valid, based on the
proof provided in the redeem. Additionally, it checks other rules that are
essential for maintaining a healthy tree state. Otherwise, the entire Adatag
service would fail.

This minting policy handles all of the system's business logic, including:

- Time-locking deposit logic (when active)
- Control NFT state validation
- Proof generation and validation
- Minting and burning logic

The rules are ordered from simplest/cheapest to most complex/expensive, but this
is not significant since the transaction with all of its inputs, outputs, and
settings is deterministic, allowing us to determine whether it will pass or fail
before submitting it.

#### Validation Logic

The Adatag minting policy performs a number of sanity checks, including:

- Validating the new state (datum in the control NFT's output) of the tree,
  including:
  - Index check: The index should always be one more than the old state in the
    input datum (index is the number of operations from bootstrap).
  - N check: The number of elements in the tree should be one more in minting
    and one less in burning (n is the old state's n in the input datum).
  - Username check: The new state's username must be a valid Adatag (non-empty,
    etc.).
- Mintinfo check: The mintinfo must contain only token with the minting policy ID
  as its symbol and the username as its token name, and it must have a value of
  +1 or -1 (depending on the minting action).

#### Lock Time Deposit Checks

- If the deactivation day has been reached, no lock time deposit checks are
  necessary.
- Otherwise, the following checks are performed:
  - The user can have a valid $adahandle in their inputs (no referene input is allowed) and outputs. If they
    do, no lock time deposit checks are necessary.
  - A new lock time deposit datum in the output UTxO must be well-formed,
    meaning that
    - the deadline is within 20 days,
    - the user's PKH is included as the beneficiary (ensuring that they are
      sending to their own address), and
    - adatag/username is also in the datum as the adatag (to ensure that each
      validation in the tx has it's own time lock deposit when it's required).

#### The Most Complex Check: Minting Script Proof

The most complex check is to pass the Adatag/username, the two components, and the minimal subtree (max 2*log2(n)*32 etc.) to the proof function, which will provide two proofs hashes (old and new) based on the minimal subtree, username, and two other components. The policy then compares these two proof hashes to the corresponding proofs stored in the relevant control NFT's datums  in the inputs and outputs.

In other words, the policy verifies that the new tree proof hash is valid and derived from the old proof hash based on whether the username has been added or removed from the adatag tree.

### The detailed mining policy rules

#### TimeLocking based rules

- (if creating) checks if lockUntil is reached (if yes, then no time locked
  deposit is necessary). (`relative cheap`)
- (if creating) Checks that $adahandles are present in both input and output and
  their token names are exactly the same as the only creational username, then
  there is not time locked deposit. (`a bit expensive`)
- (if creating) Otherwise (not present), check if lockUntil is not reached,
  checks whether a locked time output is presented with the username's public
  key hash and the correct deadline (20 days from now). (`a bit expensive`)

#### StateHolder, Control NFT and minting related rules

Contorl NFT's datum related rules

- Validates that username's first letter matches with the ouput control NFT's
  token name (StateHolder checks equality for both anyway). (`cheap if ture`)
- Validates that the input control NFT's datum is well-formed.
  (`expensive but necessary to go further`)
- Validates that the output control NFT's datum is
  well-formed.(`expensive but necessary to go further`)
- Verifies that the output NFT's datum proof != input NFT's datum proof.
  (`cheap if true`)
- Verifies that the input control NFT's datum x != username. (`cheap if true`)
- Verifies that the output control NFT's datum x == username. (`realtive cheap`)
- Verifies that the output NFT's datum n is input NFT's datum n+1 (if action is
  adding) or n-1 (if action is deleting). (`cheap`)
- Verifies that the minting value's token name is username and amount is only 1
  (if adding) or -1 (if deleting). (`cheap`)

> Note: The rules should be ordered from the least computationally expensive to
> the most, although we should always assume that Phase 2 validation will pass.

Business logic (Tree Update validation):

- Generates the proof tuple by (proofOld, proofNew) = proof(elem, nu,na,U').
  elem, nu, na, U', comes from Redeem of the minting policy attached to the tx.
  (`the most expensive operation`)
- Checks whether iNFT.datum.proof == proofOld. (`relative cheap`)
- Checks whether oNFT.datum.proof == proofNew. (`relative cheap`)
- If all of these above checks pass, then the transaction is valid.
- Otherwise, the transaction is rejected.

# Front-en (off-chain) logic

The frontend manages user inputs for username creation and deletion by reconstructing the username tree from blockchain- or cached data.

Once the tree is reconstructed, it verifies the availability of the username for specific operations, such as addition and deletion.

Following that, the frontend reconstructs the minimal subtree of the entire tree, which typically comprises approximately 2 * log2(n) elements, with 'n' representing the number of elements in the tree.

Next, a redeemer is created and passed to the transaction, containing the following data:

- 'action': adding or deleting the element.
- 'x' representing a newly created or deleted element within the tree.
- 'U' indicating the creation of the minimal subtree.
- Two additional parameters, 'na' the node to append, and 'nu'  the node to update, which will be further discussed on in subsequent sections.

Subsequently, the frontend constructs a transaction following these steps:
1. Allocation of inputs for the User:
   - For both username creation and deletion:
     - Collateral UTxO: This contains collateral to cover potential Phase 2 validation failures.
     - A control NFT located on the StateHolder address, matching the username's first letter (e.g., if the username is "ilap," the control NFT is "NFT.i"). This NFT also holds the old state of the Tree in its datum.
     - Reference script UTxOs for the StateHolder and Minting Policy (located in Always Fail validator's script addresses) are added to the inputs.
     - The user's UTxO for covering transaction fees.
   - For username creation:
     - Optionally, the user's adahandle UTxO, which contains the same username the user wants to create in @adatag. This allows the user to avoid locking a time deposit when they control the $adahandle and want to create the same @adatag username.
   - For username deletion:
     - No additional inputs are required.
2. Creation of Outputs:
   - For both username creation and deletion:
     - The relevant control NFT (e.g., NFT."i") with the updated datum reflecting the state of the tree.
     - The user's output for sending changes (if necessary).
   - For username deletion:
     - No additional outputs are necessary.
   - For username creation:
     - Time-locked deposit output:
       - If minting occurs before the lockUntil period (~3-12 months from the start of @adatag) and no $adahandle is present in the inputs. In other cases, it won't be enforced.
     - The user's adahandle output to any address (optional, for preventing time lock deposit).
     - The output of the minted username NFT (starting with "i" and exactly matching the username and with $adahandle if provided).


##  Scripts Details

| **Module** 	| **Type** 	| **Parameters** 	| **Datum** 	| **Redeemer** 	| **Comment** 	|
|---	|---	|---	|---	|---	|---	|
| Control NFT 	| Minting Policy<br>(one-shot) 	| N/A 	| void 	| N/A<br><br>When it started<br>it cannot be stopped 	| It is a one-shot NFT minting policy based on a valid spendable UTxO.<br>It mints 26 control NFT "a".."z", which will be sent, one-by-one, <br>to the StateHolder's script address. 	|
| Time Deposit 	| Validator<br>(locked deposit <br>spending) 	| N/A 	| beneficiary<br>deadline 	| N/A<br><br>As the beneficiary<br>& deadline will be<br>in the datum of the<br>spending UTxO. 	| A very simple generic time deposit validator<br>it locks deposit till deadline.<br><br>It allows spending only after a specific time by the <br>beneficiary specified in datum.<br><br><br>The creation of the time deposit  UTxO is handled by the minting policy.<br>Spending it is handled by the Time Deposit validator.<br>the minting policy handles the logic <br>(amount, beneficiary, deadline)) of it. 	|
| Minting Policy 	| Minting Policy<br>(Username creation,<br>deletion) 	| ControlNFT<br>TimeDeposit<br>AdaHandle<br>lockUntil<br>lockingPeriod<br>baseDeposit 	| N/A 	| action: add/delete<br>x: elem to add/del<br>nu: node to update<br>na: node to append<br>U: minimal tree 	| The most complex validator, it handles the:<br>1. time locking deposit logic:<br>  - no time locked deposit when a spendable $adahandle is present<br>  - the user already has this handle<br>  - the handle must not exist in the tree.<br>2. control NFT's (on state-holder address) <br>datum validity<br>3. the logic of proof generation and validation.<br>4. and minting burning logic. 	|
| StateHolder 	| Validator <br>(State carrying<br>using control NFT) 	| conrolNFT<br>mintingPolicy 	| action: added/deleted/initial<br>n: nr. of elems in the Tree<br>elem: elem added/removed<br>proof: proof of the tree 	| N/A<br><br>The StateHolder's only<br>purpose is carrying the<br>control NFT and the datum<br>(state of the tree). 	| A very simple StateHolder for carrying control NFTs:<br>1. ensures just one NFT is spent<br>2. ensures the datum is not empty<br>3. ensures that minting policy in the transaction<br><br>As minting policy will handle all the business logic 	|
