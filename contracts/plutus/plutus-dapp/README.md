# DEPRECATED IN FAVOUR OF AIKEN
## Introduction

The adatag developer has to provide:

- The one-shot "Authorization Token" minting policy to mint authorization tokens for the StateHolder
- One "Authorization Token" validator for carrying the state of the username/adatag tree state in validator address contain one authorization Token and the tree state in the datum.
- The "TimeDeposit" validator that un-locks the locked ADA deposit (when it's active).
- The "Minting" policy to mint/burn of users' adatags.

## Transactions diagrams

### 1. Mint the authorization Tokens

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
caption figure 1
title Mint authorization Tokens "a" to "z"
* Mint Authorization Tokens
** NFT."a"
** ..
** NFT."z"

left side
** UTxO

center footer Send all NFTs to the Developer
@endmindmap
```

> Note: When the NFT."a"-"z" are minted, the one-shot minting policy's currency symbol must be hardcoded in the TreeValidator.

### 2. Create the TreeValidator and Minting Policy

Constraints for creating validators:

1. The Tree validator (TV) must parameterized with the minting policy's script hash, to check its existence on moving authorization Token.
2. The TV must also need to be parameterised with the authorization Tokens currency symbol.
3. The minting NFT mist parameterised with the contol NFT currency symbol.
4.

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
title Deploy validators as Reference script

* Deploy TV and MP

** AVF: Tree Validator
** AVF: Minting Policy

left side
** UTxO

center footer They will be sent to an Always False Validator: AFV

@endmindmap
```

### 3. Deploy Labeled Tree Validator and Minting Policy as Ref script

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
title Deploy validators as Reference script

* Deploy TV and MP

** AVF: Tree Validator
** AVF: Minting Policy

left side
** UTxO

center footer They will be sent to an Always False Validator: AFV

@endmindmap
```

Note: The minting script is a parameterized script contains the

- The bitcoin's coinbase text: "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
- TreeValidator script hash,
- Initial accumulator of an empty tree and
- the empty tree itself.

### 4. Initialise authorization Tokens

Send all NFTs (one-by-one) with their initial datum to the TV script

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
* Deploy TV
** TV: NFT."a", datum: {n:0, x: "", Acc(Ta0)}
** TV: NFT..., datum: {n:0, x: "", Acc(T...0)}
** TV: NFT."z", datum: {n:0, x: "", Acc(Tz0)}

left side
** NFT."a"
** ...
** NFT."z"
@endmindmap
```

> Node: The datum contains:
>
> - The nr. of elements (`n`) of the Set, initially 0.
> - The element (`x`) added to the Set, initially an empty string (`""`),
> - The accumulator (`Acc`) of the relevant tree (`Ta`), means the username of the tree must start with `'a'`, initially with two constraints e.g. 'a' = 97 -> xa = 96 = '`'; xb = 97 = 'b':
>   - `Acc(Ta0) = H( H( n || xa || xb) || H("") || H("") )`, otherwise
>   - `Acc(Ta_i) = H( H( n_i || xa_i || xb_i) || LeftP_i || RigthP_i )`
>   - LeftP = left child's proof, RightP = rigth child's proof

### 5. Mint @adatag starts with "a"

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/

* Mint @adatag
** AFV TV: NFT."a", datum: {n', x', Acc(U')}
** User's @adatag

left side
** User UTxO \c collateral
** AFV MP: \n Redeemer : { x', nu, na, U'}
** AFV TV: NFT."a", datum: {n, x, Acc(U)}

center footer NFT."a" is a authorization Token (cNFT) sitting on the Tree Validator address \n Redemer { \n x: is the new and valid username, \n nu: updatetable node e.g. (xa, xb) ,\n na: appendable node e.g. (xa',xb'), \n U: The minimal subtree of the Ta. \n}

@endmindmap
```

### 6. Burn @adatag

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
title Burn @adatag

* Burn @adatag
** AFV TV: \n NFT."a", \n datum: {n, x, Acc(Ta)}
** User's changes
left side

** User UTxO \n NFT: @adatag
** AFV MP: \n Redeemer : { x, nu, na, U}
** AFV TV: \n NFT."a", \n datum: {n, x, Acc(Ta)}

center footer a
@endmindmap
```

### 7. Claim Locked Deposit

```plantuml
@startmindmap
!theme default from https://textchart.com/themes/
title Claim Time Locked Deposit

* Claimintg time locked value
** User's claimed ADA
** User's changes
left side

** CV: datum: {beneficiary, deadline}

center footer Anyone can send to the deposit validator (DV), \n but only the specified publick key (when datum is not present)\n or the beneficiary (when correct datum is presented) can claim them.

@endmindmap
```
