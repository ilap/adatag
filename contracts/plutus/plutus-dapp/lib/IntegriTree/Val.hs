{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

module IntegriTree.Val where

import IntegriTree.Hash
import PlutusTx qualified
import PlutusTx.Prelude hiding (toList)
import PlutusTx.Show
import Prelude qualified as Haskell

{- | Val

The `Val` type represents the constraint data as interval set used for storing and validating elements in the tree.
In a `IntegriTree`, each element must fall within the initial constraint, forming two nodes
that contain the element for every existing element in the tree.
For any non-existing but valid element (lower bound < element < upper bound),
there will be only one node. All other elements are considered invalid.
This structure ensures the generation of proofs for membership and non-membership of an element.

Membership can be validated by:
1. For membership (x ∈ T), if x equals either x' or x" in Val (x', x") of a node of the tree.
2. For non-membership (x ∉ T), if x' < x < x" in Val (x', x") of a node of the tree.
All other elements are considered invalid for the tree.
-}
data Val = Val Integer BuiltinByteString BuiltinByteString
  deriving (Haskell.Eq, Haskell.Show)

instance Eq Val where
  (Val i0 a0 b0) == (Val i1 a1 b1) = i0 == i1 && a0 == a1 && b0 == b1

-- * valid

-- It checks whether an element is valid or not.
-- valid membership, x ∈ T, if x = x' or x = x" of Val (x', x")
-- valid non-membership, x ∉ T, if x' < x < x" of Val (x', x")
-- otherwise invalid.
{-# INLINABLE valid #-}
valid :: BuiltinByteString -> Val -> Bool
valid e val = member e val || nonmember e val

-- * member

-- e == a or e == b
{-# INLINABLE member #-}
member :: BuiltinByteString -> Val -> Bool
member e (Val _ a b) = e == a || e == b

-- * nonmember

-- a < e < b
{-# INLINABLE nonmember #-}
nonmember :: BuiltinByteString -> Val -> Bool
nonmember e (Val _ a b) = a  < e && e < b

-- | The hash of the node's Value is the hash of the concatenated `xi`, `xa` and `xb` of the Val.
{-# INLINABLE hashVal #-}
hashVal :: Val -> Hash
hashVal (Val xi xa xb) = hash (intToBs xi  <> xa <> xb)

{-# INLINABLE intToBs #-}
-- It converts an Integer to a bytestring
-- >>> intToBs (-00001234567)
-- "-1234567"
-- >>> intToBs 0000001234567
-- "1234567"
--
intToBs :: Integer -> BuiltinByteString
intToBs n = encodeUtf8 (show n)

PlutusTx.unstableMakeIsData ''Val
