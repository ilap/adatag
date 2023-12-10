{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : LabeledTree
Description : A Complete Binary Tree-based data structure for data integrity verification.
License     : GPL-3

The 'LabeledTree' module provides a data structure designed for verifying data integrity, akin to Merkle trees.
It offers proofs for membership, non-membership, and updates (insertion and removal). The tree enforces uniqueness
using open intervals.

The key features include:

* **Efficient Insertion:** The insert operation has a constant time complexity (O(1)) in a complete binary tree.
  The balanced nature of the tree ensures a straightforward insertion without the need for rebalancing.

* **Construction Complexity:** Constructing a Merkle tree or a complete binary tree from a list or array has a linear
  time complexity (O(n)). The tree is built from the bottom up, with each node inserted one at a time.

* **Data Integrity Verification:** While complete binary trees excel in simplicity and efficient insertion, Merkle trees
  offer advantages for data integrity verification. They can be used to ensure the integrity of data through proofs.

Example Usage:

> -- Creating a labeled binary tree
> tree = LabeledNode (Val 1 "LabelA" "LabelB") LabeledEmpty LabeledEmpty

> -- Inserting a new node into the tree
> updatedTree = insertNode (Val 2 "NewLabel" "Data") tree

> -- Verifying data integrity using proofs
> proof = generateProof updatedTree
> isIntegrityVerified = verifyProof proof
-}
module LabeledTree.LabeledTree where

import LabeledTree.Hash
import LabeledTree.Val
import PlutusPrelude hiding (toList)
import PlutusTx qualified
import PlutusTx.Prelude hiding (toList)
import Prelude qualified as Haskell

-- * LabeledTree

{- | The 'LabeledTree' is a Complete Binary Tree-based data structure designed for verifying data integrity,
similar to Merkle trees. It includes proofs for membership, non-membership, and updates (insertation into
and removal from the tree).
It uses open interval for enforce uniqueness of the elements in the tree.
-}
data LabeledTree
  = LabeledEmpty
  | LabeledNode Val LabeledTree LabeledTree
  deriving (Haskell.Eq, Haskell.Show)

instance Eq LabeledTree where
  (==) :: LabeledTree -> LabeledTree -> Bool
  LabeledEmpty == LabeledEmpty = True
  (LabeledNode v0 _ _) == (LabeledNode v1 _ _) = v0 == v1
  _ == _ = False

size :: LabeledTree -> Integer
size LabeledEmpty = 0
size (LabeledNode _ l r) = 1 + size l + size r

{- | Construct a 'LabeledTree' from a list of elements (`[Val]`) using level-order construction.

The time- and space complexity of the algorithm is O(n) as we iterate through elements
of the binary tree for level order traversal only once.
-}
fromList :: [Val] -> LabeledTree
fromList xs = go 1
  where
    len = length xs
    go idx
      | idx > len = LabeledEmpty
      | otherwise =
          let Val _ a' b' = xs !! (idx - 1)
              lnode = go (2 * idx)
              rnode = go (2 * idx + 1)
           in LabeledNode (Val idx a' b') lnode rnode

{- | Deconstruct a 'LabeledTree' back to a list of elements (`[Val]`) using
the Level Order Traversal (Breadth-First Search or BFS).

>>> toList (fromList xs) == xs
True
-}
toList :: LabeledTree -> [Val]
toList tree = go [tree]
  where
    go [] = []
    go (LabeledEmpty : rest) = go rest
    go (LabeledNode val left right : rest) = val : go (rest ++ [left, right])

-- | Obtain the root hash of a 'LabeledTree'.
rootHash :: LabeledTree -> Hash
rootHash =
  \case
    LabeledEmpty -> hash ""
    LabeledNode val l r ->
      combineThreeHashes (hashVal val) (rootHash l) (rootHash r)

{- | Finds the node for a specified value.

It returns a node that contains the provided `Val` otherwise it returns Nothing.
-}
findNodeByVal :: LabeledTree -> Val -> Maybe LabeledTree
findNodeByVal LabeledEmpty _ = Nothing
findNodeByVal node@(LabeledNode v left right) targetVal
  | v == targetVal = Just node
  | otherwise = findNodeByVal left targetVal <|> findNodeByVal right targetVal

{- | Finds the node for a specified value.

It returns a node whose Val is valid for the element, otherwise it returns Nothing.
I.e.: x = x' or x = x" of the Val (x', x") when x ∈ T, or x' < x < x" when x ∉ T
-}
findValidNode :: LabeledTree -> BuiltinByteString -> Maybe LabeledTree
findValidNode LabeledEmpty _ = Nothing
findValidNode node@(LabeledNode val left right) e
  | valid e val = Just node
  | otherwise = findValidNode left e <|> findValidNode right e

{- | Finds the lowest common ancestor (LCA) of two nodes.

It takes the tree and two nodes to find their lowest common ancestor if any

It returns the LCA node of an empty node if no LCA.
-}
findLca :: LabeledTree -> LabeledTree -> LabeledTree -> LabeledTree
findLca LabeledEmpty _ _ = LabeledEmpty
findLca root@(LabeledNode _ left right) n1 n2
  | root == n1 || root == n2 = root
  | otherwise = case (l, r) of
      (LabeledNode {}, LabeledNode {}) -> root
      (_, _) -> if l /= LabeledEmpty then l else r
  where
    l = findLca left n1 n2
    r = findLca right n1 n2

PlutusTx.unstableMakeIsData ''LabeledTree
