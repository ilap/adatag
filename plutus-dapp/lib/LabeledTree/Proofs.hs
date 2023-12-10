{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

module LabeledTree.Proofs where

import LabeledTree.Hash
import LabeledTree.Val
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.Prelude hiding (toList)
import Prelude qualified as Haskell

{- | `ProofTree` is the compact version of a LabeledTree used for constructing proofs.

It consists of nodes representing either a leaf node with a hash value (`ProofLeaf`)
or an internal node with a labeled value (`ProofNode`) and its corresponding proof subtrees.
-}
data ProofTree
  = ProofLeaf Hash
  | ProofNode Val ProofTree ProofTree
  deriving (Haskell.Show)

-- The 'emptyHash' value is computed once at runtime and can be used as a constant
-- TODO: No-inline if possible
emptyHash :: Hash
emptyHash = hash ""

rootHash' :: ProofTree -> Hash
rootHash' tree = case tree of
  ProofLeaf h -> h
  ProofNode v l r -> combineThreeHashes (hashVal v) (rootHash' l) (rootHash' r)

{- | `appendNodes` is a function that creates the updated append nodes from the
two nodes (update and append node) that will be modified (updated and appended) in the tree
during the insertion of an element.

The function takes a BuiltinByteString, an update node (`nu`), an append node (`na`),
and an index (`idx`). It returns a tuple containing the updated update node,
the updated append node, and the new leaf for insertion.
-}
{-# INLINABLE appendNodes #-}
appendNodes :: BuiltinByteString -> Val -> Val -> Integer -> (Val, Val, Val)
appendNodes x nu@(Val i a b) na idx =
  if a < x && x < b
    then (nu', na', leaf)
    else PlutusTx.Prelude.traceError "invalid update node"
  where
    nu' = Val i a x
    -- Clone it
    na' = if nu == na then nu else na
    leaf = Val idx x b

{- | `checkAppend` is a function designed to validate the correctness of a tree after an insertion.

It takes the following inputs:
  1. The element being inserted (as a `BuiltinByteString`).
  2. The size of the tree (required for enforcing the completeness of the tree).
  3. The old root hash of the tree before insertion.
  4. The updated root hash of the tree after insertion.
  5. The value of the update node in the tree before insertion.
  6. The value of the append node in the tree before insertion.
  7. A proof, represented by a minimal subtree containing the two nodes whose values are used,
     validating the insertion operation.

The function checks two scenarios:
  a. When `False` is passed to the `checkProof`, it validates the root hash before insertion.
  b. When `True` is passed to the `checkProof`, it validates the root hash of the tree after insertion.

| `checkAppend` verifies the integrity of a tree by confirming that:
  - The two nodes' values are present in the proof (a minimal subtree of the tree).
  - The old root hash can be regenerated using the values of the update and append nodes and the provided proof.
  - The updated root hash can be regenerated by modifying these nodes based on the passed element and using the same proof tree.

This function is used to prove that an element has been inserted into the tree, with the old root hash representing the state before insertion
and the updated root hash representing the state after insertion.

This allows users to securely prove that they've successfully inserted the specified element into the tree.

The function returns `True` if both scenarios pass; otherwise, it returns `False`.

> Note: The tsize and the old root hash is a public value derived from previous operations and cannot be manipulated.
> Also, the element and the vals must be checked before passing to the checkAppend function.

Example Usage:

>>> let result = checkAppend "element" 10 oldRootHash updatedRootHash updateNodeVal appendNodeVal proofTree
print result -- Should print True if the proof is valid.
-}
{-# INLINABLE checkAppend #-}
checkAppend :: BuiltinByteString -> Integer -> Hash -> Hash -> Val -> Val -> ProofTree -> Bool
checkAppend e tsize rootHash updatedRootHash nuv nav proof =
  --  'False': Used when the root hash before insertion must be validated.
  --  'True': Used when the root hash of the tree after insertion must be validated.
  checkProof False rootHash -- && trace "b" checkProof True updatedRootHash
  where
    nuHash = hashVal nuv
    naHash = hashVal nav
    (nuv', nav', leaf) = appendNodes e nuv nav tsize

    checkProof isUpdated root = hasNu && hasNa && root' == root
      where
        go hasNu' hasNa' (ProofLeaf h) = trace "l" (hasNu', hasNa', h)
        go hasNu' hasNa' (ProofNode v l r)
          | isUpdated && isAny = trace "a" (lnu || rnu, lna || rna, combineThreeHashes nvh nlh nrh)
          | otherwise = trace "b" (lnu || rnu, lna || rna, combineThreeHashes vh lh rh)
          where
            vh = trace "x" $ hashVal v
            hasNu'' = hasNu' || vh == nuHash
            hasNa'' = hasNa' || vh == naHash

            (lnu, lna, lh) = go hasNu'' hasNa'' l
            (rnu, rna, rh) = go hasNu'' hasNa'' r

            isAny = vh == nuHash || vh == naHash
            leafh = combineThreeHashes (hashVal leaf) emptyHash emptyHash
            nvh = if vh == nuHash then hashVal nuv' else hashVal nav'
            (nlh, nrh) = if lh == emptyHash then (leafh, rh) else (lh, leafh)

        (hasNu, hasNa, root') = go False False proof

PlutusTx.unstableMakeIsData ''ProofTree
