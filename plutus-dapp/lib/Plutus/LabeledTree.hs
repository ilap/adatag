{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Plutus.LabeledTree where

import qualified Data.ByteString.Base16 as Haskell.Base16
import qualified Data.Text as Haskell.Text
import qualified Data.Text.Encoding as Haskell.Text.Encoding
import PlutusPrelude hiding (toList)
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell
import           Text.Printf                (printf)

-- * LabeledTree

-- | The 'LabeledTree' is a Complete Binary Tree-based data structure designed for verifying data integrity,
-- similar to Merkle trees. It includes proofs for membership, non-membership, and updates (insertation into
-- and removal from the tree).
data LabeledTree
  = LabeledEmpty
  | LabeledNode Val LabeledTree LabeledTree
  deriving (Haskell.Eq, Haskell.Show)

instance Eq LabeledTree where
  (==) :: LabeledTree -> LabeledTree -> Bool
  LabeledEmpty == LabeledEmpty = True
  (LabeledNode v0 _ _) == (LabeledNode v1 _ _) = v0 == v1
  _ == _ = False

{-# INLINEABLE size #-}
size :: LabeledTree -> Integer
size LabeledEmpty = 0
size (LabeledNode _ l r) = 1 + size l + size r

-- * HashNode a node structure with proofs as it's left and right

data HashNode = HashNode Val Hash Hash
  deriving (Haskell.Eq, Haskell.Show)

instance Eq HashNode where
  (==) :: HashNode -> HashNode -> Bool
  (HashNode v h1 h2) == (HashNode v' h1' h2') = v == v' && h1 == h1' && h2 == h2'

-- | Val
--
-- The `Val` type represents the constraint data used for storing and validating elements in the tree.
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
data Val = Val
  { a :: BuiltinByteString,
    b :: BuiltinByteString
  }
  deriving (Haskell.Eq, Haskell.Show)

instance Eq Val where
  (Val a0 b0) == (Val a1 b1) = a0 == a1 && b0 == b1

-- | Construct a 'LabeledTree' from a list of elements (`[Val]`) using level-order construction.
--
-- The time- and space complexity of thethe algorithm is O(n) as we iterate through elements
-- of the binary tree for level order traversal only once.
{-# INLINEABLE fromList #-}
fromList :: [Val] -> LabeledTree
fromList xs = go 1
  where
    len = length xs
    go idx
      | idx > len = LabeledEmpty
      | otherwise =
          let Val a' b' = xs !! (idx - 1)
              lnode = go (2 * idx)
              rnode = go (2 * idx + 1)
           in LabeledNode (Val a' b') lnode rnode

-- | Deconstruct a 'LabeledTree' back to a list of elements (`[Val]`) using
-- the Level Order Traversal (Breadth-First Search or BFS).
--
-- >>> toList (fromList xs) == xs
-- True
toList :: LabeledTree -> [Val]
toList tree = go [tree]
  where
    go [] = []
    go (LabeledEmpty : rest) = go rest
    go (LabeledNode val left right : rest) = val : go (rest ++ [left, right])

-- * valid

-- It checks whether an element is valid or not.
-- membership, x ∈ T, if x = x' or x = x" of Val (x', x")
-- non-membership, x ∉ T, if x' < x < x" of Val (x', x")
-- otherwise invalid.
{-# INLINEABLE valid #-}
valid :: BuiltinByteString -> Val -> Bool
valid e val = member e val || nonmember e val

-- * member

-- e == a or e == b
{-# INLINEABLE member #-}
member :: BuiltinByteString -> Val -> Bool
member e val = e == a val || e == b val

-- * nonmember

-- a < e < b
{-# INLINEABLE nonmember #-}
nonmember :: BuiltinByteString -> Val -> Bool
nonmember e val = a val < e && e < b val

-- | The hash of the node's Value is the hash of the concatenated `a` and `b` of the Val.
{-# INLINEABLE hashVal #-}
hashVal :: Val -> Hash
hashVal v = hash (a v <> b v)

-- | The membership & non-membership 'Proof'.
newtype Proof
  = Proof (HashNode, [Either Hash Hash])
  deriving (Haskell.Show)

-- | Constructs a 'Proof' for membership or non-membership of an element in the tree.
--
-- Returns 'Nothing' if the element is invalid, indicating that it does not belong
-- to the set of strings 's' where the constraint is that `lower bound < s < upper bound`.
-- It's using lexicographical order comparison.
--
-- The constraints (lower and upper bounds) are specified by an initial empty tree, for example:
--
-- >>> fromList [Val "`" "{"] -- meaning only strings that starts with lowercase "a"-"z" are valid.
-- LabeledNode (Val {a = "`", b = "{"}) LabeledEmpty LabeledEmpty
-- Todo : We should use some data Proof = [Maybe Val, Either Hash Hash] instead, for root and node's proof.
{-# INLINEABLE mkProof #-}
mkProof :: BuiltinByteString -> LabeledTree -> Maybe Proof
mkProof e = go []
  where
    go es =
      \case
        LabeledEmpty -> Nothing
        LabeledNode val l r
          | valid e val -> Just $ Proof (HashNode val (proof l) (proof r), es)
          | otherwise -> go (Right (proof r) : es) l <|> go (Left (proof l) : es) r

-- | Validates the membership status of an element using the root hash of the tree
-- and a provided 'Proof'.
-- The efficiency of the proof verification is logarithmic in the size of the tree.
{-# INLINEABLE membership #-}
membership :: BuiltinByteString -> Val -> Hash -> Maybe Proof -> Bool
membership _ _ _ Nothing = False
membership e rootVal root (Just (Proof (HashNode val lh rh, p))) =
  let vh = combineThreeHashes (hashVal val) lh rh
      rvh = hashVal rootVal
      -- Verifying a membership/non-membership 'Proof' for a given element in a LabeledTree
      -- involves combining hashes at each level. Triple hashes are used at the root
      -- and node levels, while double hashes are used at other levels (like in the merkle trees).
      --
      -- For the root and node level: H( Hash(node/root's Val) || left proof || right proof )
      -- For other levels: H( left proof || right proof) where one of these proofs
      -- are the calculated (initially at node's level) and the other is the provided from the Proof's [Either Hash Haah].
      go root' lh' rh' =
        \case
          [] -> root == combineThreeHashes rvh lh' rh'
          Left l : q -> go (combineHashes l root') l root' q
          Right r : q -> go (combineHashes root' r) root' r q
   in valid e val && go vh lh rh p

-- | Obtain the proof (hash) of a 'LabeledTree'.
{-# INLINEABLE proof #-}
proof :: LabeledTree -> Hash
proof =
  \case
    LabeledEmpty -> hash ""
    LabeledNode val l r ->
      combineThreeHashes (hashVal val) (proof l) (proof r)

-- * Hash

newtype Hash
  = Hash BuiltinByteString
  deriving (Haskell.Eq)

instance Eq Hash where
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) =
    Haskell.Text.unpack
      . Haskell.Text.Encoding.decodeUtf8
      . Haskell.Base16.encode
      . fromBuiltin
      . takeByteString 3
      $ bs

{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256

{-# INLINEABLE combineHashes #-}
combineHashes :: Hash -> Hash -> Hash
combineHashes (Hash h) (Hash h') = hash (appendByteString h h')

-- * combineThreeHashes

{-# INLINEABLE combineThreeHashes #-}
combineThreeHashes :: Hash -> Hash -> Hash -> Hash
combineThreeHashes (Hash h) (Hash h') (Hash h'') =
  hash (appendByteString h (appendByteString h' h''))

-- | Finds the node for a specified value.
--
-- It returns a node that contains the provided `Val` otherwise it returns Nothing.
findNodeByVal :: LabeledTree -> Val -> Maybe LabeledTree
findNodeByVal LabeledEmpty _ = Nothing
findNodeByVal node@(LabeledNode v left right) targetVal
  | v == targetVal = Just node
  | otherwise = findNodeByVal left targetVal <|> findNodeByVal right targetVal

-- | Finds the node for a specified value.
--
-- It returns a node whose Val is valid for the element, otherwise it returns Nothing.
-- I.e.: x = x' or x = x" of the Val (x', x") when x ∈ T, or x' < x < x" when x ∉ T
findNode :: LabeledTree -> BuiltinByteString -> Maybe LabeledTree
findNode LabeledEmpty _ = Nothing
findNode node@(LabeledNode val left right) e
  | valid e val = Just node
  | otherwise = findNode left e <|> findNode right e

PlutusTx.unstableMakeIsData ''Hash

PlutusTx.unstableMakeIsData ''Val

PlutusTx.unstableMakeIsData ''LabeledTree

---------------------------------------------------------------------------------------------------
------------------------------- HELPER FUNCTIONS FOR TESTIMG --------------------------------------
-- ghci> import Plutus.LabeledTree
-- ghci> import Data.Maybe (fromMaybe)
-- ghci> :set -XOverloadedStrings
-- ghci> proof $ fromMaybe LabeledEmpty $ findNodeByVal sampleTree (Plutus.LabeledTree.Val "g" "h")
-- c39e8c972fa8
-- ghci> import Plutus.LabeledTree
-- ghci> :set -XOverloadedStrings
-- ghci> let m = mkProof "z" sampleTree
-- ghci> import Data.Maybe (fromJust)
-- ghci> let Proof (HashNode v l r, p) = fromJust mp
-- ghci> ghci> combineThreeHashes (hash (a v <> b v)) l r
-- 2fe3a179f298d5769faa557c29ee6f7e0d48d98b7e6ea8b92d3d1a4141db9860
-- Tracing the code
-- rh = Debug.Trace.trace ("Hash" ++ Haskell.show (hash "")) $ hash ""
sampleVals :: [Val]
sampleVals =
  [ Val "`" "c",
    Val "z" "{",
    Val "f" "g",
    Val "j" "l",
    Val "d" "e",
    Val "y" "z",
    Val "g" "h",
    Val "l" "o",
    Val "u" "v",
    Val "s" "u",
    Val "x" "y",
    Val "e" "f",
    Val "o" "s",
    Val "i" "j",
    Val "v" "x",
    Val "c" "d",
    Val "h" "i"
  ]

-- Create a sample tree from the list of Vals
sampleTree :: LabeledTree
sampleTree = fromList sampleVals


log2 :: Integer -> Integer
log2 x = if odd x then 0 else (Haskell.floor . Haskell.logBase (2 :: Haskell.Double) . Haskell.fromIntegral) x

printTree :: LabeledTree -> Haskell.IO ()
printTree LabeledEmpty = Haskell.putStr ""
printTree n =
  go n 1 "" level-- npr  
  where
    s = size n
    level = (Haskell.floor . Haskell.sqrt . Haskell.fromIntegral ) s 
    npr = "--"

    go :: LabeledTree -> Integer -> Haskell.String -> Integer -> Haskell.IO ()
    go LabeledEmpty _ _ _= Haskell.putStr ""
    go node@(LabeledNode val left right) i p l = do

      Haskell.putStrLn $ p ++ "| " ++ printf "%*d" (l*2+1) i ++ " (" ++ Haskell.show xa ++ ", " ++ Haskell.show xb ++ "), val hash: " ++
        Haskell.show vh ++ ", proof: " ++ Haskell.show pn ++ ", pL: " ++
        Haskell.show pl ++ ", pR: " ++ Haskell.show pr
      go left (2*i) (p ++ npr) (l-1)
      go right (2*i+1) (p ++ npr) (l-1)
        where
          xa = a val
          xb = b val
          vh = hashVal val
          pn = proof node
          pl = proof left
          pr = proof right


