{-# LANGUAGE TemplateHaskell #-}

module LabeledTree.Val where

import LabeledTree.Hash
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell
import qualified PlutusLedgerApi.V2 as SB
import PlutusTx.Show

-- | Val
--
-- The `Val` type represents the constraint data as interval set used for storing and validating elements in the tree.
-- In a `LabeledTree`, each element must fall within the initial constraint, forming two nodes
-- that contain the element for every existing element in the tree.
-- For any non-existing but valid element (lower bound < element < upper bound),
-- there will be only one node. All other elements are considered invalid.
-- This structure ensures the generation of proofs for membership and non-membership of an element.
--
-- Membership can be validated by:
-- 1. For membership (x ∈ T), if x equals either x' or x" in Val (x', x") of a node of the tree.
-- 2. For non-membership (x ∉ T), if x' < x < x" in Val (x', x") of a node of the tree.
-- All other elements are considered invalid for the tree.
data Val
  = Val
      { xi :: Integer,
        xa :: BuiltinByteString,
        xb :: BuiltinByteString
      }
  deriving (Haskell.Eq, Haskell.Show)

instance Eq Val where
  (Val i0 a0 b0) == (Val i1 a1 b1) = i0 == i1 && a0 == a1 && b0 == b1

-- * valid

-- It checks whether an element is valid or not.
-- valid membership, x ∈ T, if x = x' or x = x" of Val (x', x")
-- valid non-membership, x ∉ T, if x' < x < x" of Val (x', x")
-- otherwise invalid.
{-# INLINEABLE valid #-}
valid :: BuiltinByteString -> Val -> Bool
valid e val = member e val || nonmember e val

-- * member

-- e == a or e == b
{-# INLINEABLE member #-}
member :: BuiltinByteString -> Val -> Bool
member e val = e == xa val || e == xb val

-- * nonmember

-- a < e < b
{-# INLINEABLE nonmember #-}
nonmember :: BuiltinByteString -> Val -> Bool
nonmember e val = xa val < e && e < xb val

-- | The hash of the node's Value is the hash of the concatenated `xi`, `xa` and `xb` of the Val.
{-# INLINEABLE hashVal #-}
hashVal :: Val -> Hash
hashVal v = hash ((intToBs $ xi v) <> xa v <> xb v)



{-# INLINEABLE intToBs #-}
-- It converts an Integer to a bytestring
-- >>> intToBs (-00001234567)
-- "-1234567"
-- >>> intToBs 0000001234567
-- "1234567"
--
intToBs :: Integer -> BuiltinByteString
intToBs n =  encodeUtf8 (show n)

PlutusTx.unstableMakeIsData ''Val