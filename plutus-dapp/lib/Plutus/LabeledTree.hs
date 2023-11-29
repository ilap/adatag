{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Plutus.LabeledTree where

import qualified Data.ByteString.Base16 as Haskell.Base16
import qualified Data.Text as Haskell.Text
import qualified Data.Text.Encoding as Haskell.Text.Encoding
import PlutusPrelude hiding (toList)
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import Text.Printf (printf)
import qualified Prelude as Haskell

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
  = Proof (HashNode, [(Hash, Either Hash Hash)])
  deriving (Haskell.Show)

-- * HashNode a node structure with proofs as it's left and right

data HashNode = HashNode Val Hash Hash
  deriving (Haskell.Eq, Haskell.Show)

instance Eq HashNode where
  (==) :: HashNode -> HashNode -> Bool
  (HashNode v h1 h2) == (HashNode v' h1' h2') = v == v' && h1 == h1' && h2 == h2'

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
-- Todo : We should use some data Proof = [(Maybe ValHash, Either Hash Hash)] instead, for root and node's proof.
{-# INLINEABLE mkProof #-}
mkProof :: BuiltinByteString -> LabeledTree -> Maybe Proof
mkProof e = go []
  where
    go es = \case
      LabeledEmpty -> Nothing
      LabeledNode val l r
        | valid e val ->
            let nl = HashNode val (proof l) (proof r)
             in Just $ Proof (nl, es)
        | otherwise ->
            let vh = hashVal val
             in go ((vh, Right (proof r)) : es) l <|> go ((vh, Left (proof l)) : es) r

-- | Validates the membership status of an element using the root hash of the tree
-- and a provided 'Proof'.
-- The efficiency of the proof verification is logarithmic in the size of the tree.

{-# INLINEABLE membership #-}
membership :: BuiltinByteString -> Hash -> Maybe Proof -> Bool
membership _ _ Nothing = False
membership e root (Just (Proof (HashNode val lh rh, p))) =
  let vh = combineThreeHashes (hashVal val) lh rh
      -- Verifying a membership/non-membership 'Proof' for a given element in a LabeledTree
      -- involves combining hashes at each level. Triple hashes are used at every levels.
      go root' =
        \case
          [] -> root == root'
          (vh', Left l) : q -> go (combineThreeHashes vh' l root') q
          (vh', Right r) : q -> go (combineThreeHashes vh' root' r) q
  in valid e val && go vh p

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
-- ghci> let Proof ( v l r, p) = fromJust mp
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

sampleVals2 :: [Val]
sampleVals2 = [Val "`" "adam_1969", Val "susan012" "sylvan_37", Val "mike-a210" "mystic_33", Val "adam_1969" "alex_omega", Val "angel012" "aqua_69", Val "pedre_33" "phoenix_12", Val "luna_23" "luna_55", Val "alex_omega" "angel012", Val "bella_8" "casc-ada", Val "harmony-42" "harmony-88", Val "david_123" "dreamer_22", Val "violet-22" "whispering_wolf", Val "james5" "jason_13", Val "whispering_wolf" "whispers_12", Val "emma_11" "enchanted_66", Val "enchanted_66" "enigma_12", Val "johnny_9" "journey_23", Val "mystic_33" "mystic_8", Val "phoenix_77" "quasar_33", Val "sapphire_8" "seraph_22", Val "stardust_23" "susan012", Val "olivia_99" "pedre_33", Val "journeyer_44" "legend_22", Val "chris_7" "david_123", Val "tranquility_12" "tranquility_14", Val "melody_56" "meridian_12", Val "celestial_81" "chris_7", Val "infinity_3" "james5", Val "nebula_17" "nebula_88", Val "legend_22" "lullaby_22", Val "obsidian_10" "oliver_6", Val "aurora_92" "bella_8", Val "grace_14" "harmony-14", Val "cascade-19" "cascade-27", Val "michael_8" "mike-a210", Val "zen_8" "zenith_11", Val "nova_45" "obsidian_10", Val "luminary_6" "luminary_67", Val "harmony-14" "harmony-42", Val "nebula_88" "nebulous_61", Val "seraph_27" "seraph_33", Val "aqua_69" "astral001", Val "ethereal_11" "ethereal_22", Val "cascade-27" "cascade_12", Val "oliver_6" "olivia_99", Val "whispers_22" "whispers_36", Val "tempest_44" "tempest_67", Val "trinity_8" "violet-22", Val "jason_13" "johnny_9", Val "astral_77" "astral_9", Val "illusion_9" "infinity_19", Val "solstice_21" "stardust_23", Val "sylvan_37" "tempest_16", Val "eternal_4" "ethereal_11", Val "journey_23" "journeyer_44", Val "serenade_15" "serendipity_7", Val "enigma_18" "enigma_37", Val "dreamweaver_25" "dreamweaver_36", Val "drizzle_7" "drizzle_91", Val "aurora_45" "aurora_6", Val "harmony-88" "illusion_76", Val "solitude_14" "solstice_18", Val "infinity_19" "infinity_3", Val "zenith_99" "zephyr_12", Val "mariner_23" "melody_56", Val "nocturne_42" "nova_45", Val "ethereal_8" "etherme", Val "phoenix_55" "phoenix_76", Val "mystique_12_1" "mystique_12_2", Val "luminous_32" "luna_14", Val "cascade_77" "cascade_8", Val "zephyr_4" "zephyr_55", Val "whispers_66" "whispers_67", Val "ember_18" "emma_11", Val "seraphim_9" "serenade_15", Val "lunar_37" "lunar_61", Val "meridian_12" "michael_8", Val "nebulous_61" "nocturnal1985", Val "celestial_14" "celestial_19", Val "quasar_33" "quasar_36", Val "radiant_76" "radiant_77", Val "drifter_8" "drizzle_7", Val "solace_22" "solace_92", Val "echoes_43" "echoes_8", Val "tranquility_67" "tranquility_9", Val "siren_12" "siren_77", Val "astral_9" "aurora-02", Val "zenith_11" "zenith_77", Val "dreamer_88" "dreamer_99", Val "serendipity_7" "serenity_14", Val "cascade_55" "cascade_77", Val "luminosity_14" "luminous_32", Val "mystic_8" "mystique_12_1", Val "drizzle_91" "echo-es01", Val "zenith_77" "zenith_99", Val "lullaby_5" "lullaby_7", Val "ethereal_22" "ethereal_33", Val "whispers_61" "whispers_66", Val "enigma_37" "enigma_67", Val "lunar_13" "lunar_37", Val "tempest_16" "tempest_44", Val "phoenix_29" "phoenix_55", Val "radiant_44" "radiant_76", Val "serenity_23" "serenity_33", Val "solstice_18" "solstice_21", Val "illusion_76" "illusion_77", Val "celestial_19" "celestial_67", Val "cascade_9" "celestial_12", Val "tranquility_36" "tranquility_67", Val "zephyr_88" "{", Val "whispers_12" "whispers_22", Val "seraph_57" "seraphim_9", Val "luminary_33" "luminary_6", Val "dreamweaver_21" "dreamweaver_25", Val "aurora_6" "aurora_92", Val "echoes_8" "ember_18", Val "ethereal_47" "ethereal_8", Val "solace_92" "solitude_14", Val "astral_17" "astral_77", Val "quasar_55" "quasar_67", Val "radiant_33" "radiant_44", Val "luna_14" "luna_23", Val "nocturnal_61" "nocturne_42", Val "mystique_12_2" "mystique_12_3", Val "siren_77" "siri001", Val "cascade_12" "cascade_36", Val "serenity_44" "shoelace004", Val "tempest_8" "tempike", Val "dreamer_99" "dreamweaver_21", Val "lullaby_22" "lullaby_5", Val "ethereal_37" "ethereal_47", Val "whispers_67" "whispers_8", Val "enigma_12" "enigma_18", Val "lunar_61" "lunar_9", Val "phoenix_76" "phoenix_77", Val "rada-007" "rada007", Val "illusion_77" "illusion_8", Val "celestial_67" "celestial_81", Val "cascade_36" "cascade_55", Val "tranquility_9" "trinity_8", Val "zephyr_12" "zephyr_4", Val "whispers_88" "zen_8", Val "seraph_33" "seraph_57", Val "luminary_14" "luminary_33", Val "dreamweaver_36" "dreamweaver_67", Val "aurora-02" "aurora002", Val "echo-es01" "echoes01", Val "ethereal_33" "ethereal_37", Val "shoelace1" "siren_12", Val "astral001" "astral1", Val "quasar_36" "quasar_55", Val "radiant_88" "sapphire_8", Val "luna_55" "luna_67", Val "nocturnal_12" "nocturnal_61", Val "mystique_12_3" "mystique_12_4", Val "siri001-02" "solace_22", Val "cascade_8" "cascade_9", Val "serenity_33" "serenity_44", Val "tempike" "tranquility_12", Val "dreamer_22" "dreamer_88", Val "lullaby_7" "luminary_14", Val "etherme" "etherme2", Val "whispers_36" "whispers_61", Val "enigma_67" "eternal_4", Val "lunar_9" "mariner_23", Val "phoenix_12" "phoenix_29", Val "radiant_77" "radiant_88", Val "illusion_8" "illusion_9", Val "celestial_12" "celestial_14", Val "casc-ada" "cascada9", Val "tranquility_14" "tranquility_36", Val "zephyr_55" "zephyr_88", Val "whispers_8" "whispers_88", Val "seraph_22" "seraph_27", Val "luminary_67" "luminosity_14", Val "dreamweaver_67" "drifter_8", Val "aurora002" "aurora_45", Val "echoes01" "echoes_43", Val "etherme2" "grace_14", Val "shoelace004" "shoelace1", Val "astral1" "astral_17", Val "quasar_67" "rada-007", Val "rada007" "radiant_33", Val "luna_67" "lunar_13", Val "nocturnal1985" "nocturnal_12", Val "mystique_12_4" "nebula_17", Val "siri001" "siri001-02", Val "cascada9" "cascade-19", Val "serenity_14" "serenity_23", Val "tempest_67" "tempest_8"]

-- Create a sample tree from the list of Vals
sampleTree :: LabeledTree
sampleTree = fromList sampleVals

log2 :: Integer -> Integer
log2 x = if odd x then 0 else (Haskell.floor . Haskell.logBase (2 :: Haskell.Double) . Haskell.fromIntegral) x

printTree :: LabeledTree -> Haskell.IO ()
printTree LabeledEmpty = Haskell.putStr ""
printTree n =
  go n 1 "" level -- npr
  where
    s = size n
    level = (Haskell.floor . Haskell.sqrt . Haskell.fromIntegral) s
    ppr = "+"
    npr = "-"

    go :: LabeledTree -> Integer -> Haskell.String -> Integer -> Haskell.IO ()
    go LabeledEmpty _ _ _ = Haskell.putStr ""
    go node@(LabeledNode val left right) i p l = do
      let pf = if even i && i /= 0 then npr else ppr

      Haskell.putStrLn
        $ p
        ++ pf
        ++ " "
        ++ printf "%*d" (l * 2 + 1) i
        ++ " ("
        ++ Haskell.show xa
        ++ " "
        ++ Haskell.show xb
        ++ ", Valhash: "
        ++ Haskell.show vh
        ++ ", proof: "
        ++ Haskell.show pn
        ++ ", pL: "
        ++ Haskell.show pl
        ++ ", pR: "
        ++ Haskell.show pr
      go left (2 * i) (p ++ ppr) (l - 1)
      go right (2 * i + 1) (p ++ ppr) (l - 1)
      where
        xa = a val
        xb = b val
        vh = hashVal val
        pn = proof node
        pl = proof left
        pr = proof right
