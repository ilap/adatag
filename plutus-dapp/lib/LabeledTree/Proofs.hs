{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LabeledTree.Proofs where

import LabeledTree.Hash
import LabeledTree.LabeledTree
import LabeledTree.Val
import PlutusPrelude hiding (toList)
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

-- The HashNode is the compact version of a LabeledNodeused for constructing proofs.
data HashNode = HashNode Val Hash Hash
  deriving (Haskell.Eq, Haskell.Show)

instance Eq HashNode where
  (HashNode v h1 h2) == (HashNode v' h1' h2') = v == v' && h1 == h1' && h2 == h2'

-- | Represents a proof for a (sub)tree. Consists of the main proof data
-- and a list of hash-related information.
data TreeProof = TreeProof (Proof, [(Hash, Either Hash Hash)])
  deriving (Haskell.Show)

-- | Represents a proof for a value. Can be either a direct value (`ValProof`)
-- or a hash of the value (`ValHash`).
data ValProof
  = ValProof Val
  | ValHash Hash
  deriving (Haskell.Show)

-- | Represents a proof for a node. Can be either a direct hash node (`NodeProof`)
-- or a hash of the node (`NodeHash`).
data NodeProof
  = Node HashNode
  | NodeHash Hash
  deriving (Haskell.Show)

-- | Represents a generic proof, which can be either a proof for a value (`ValProof`)
-- or a proof for a node (`NodeProof`).
data Proof
  = PartialProof ValProof
  | FullProof NodeProof
  deriving (Haskell.Show)

-- | Represents the type of the generic proof for function parameters.
--
-- TODO: the Proof and ProofType should be consolidated to some record type.
data ProofType = Partial | Full

-- | hashNode creates a compact form of a LabeledNode
-- It returns a HashNode or trace errors
-- Off-chain only
hashNode :: LabeledTree -> HashNode
hashNode LabeledEmpty = traceError "Empty node cannot be a hash node" -- TODO: XXX HashHash $ hash ""
hashNode (LabeledNode val left right) = HashNode val (rootHash left) (rootHash right)

-- | The membership & non-membership 'TreeProof'.
--
-- The proof is a tuple of a node existing tree and the list of tuples [(Val hash, Node hash)] that contain the
-- label of the node at the level whose proof needs to be calculated and the hash of the sibling.
--
-- The example below to calculate the left and right node's proof at level for is:
-- the node hash (left or right) and the opposite side level 5's H (ValHash || left proof || right proof )
--   Lvl 4.   | Val hash  | left proof | right proof |                | Node hash |
--                              /              \
--   Lvl 5.        | Node hash |           | Val Hash | left proof | right proof |
-- newtype TreeProof
--  = TreeProof (HashNode, [(Hash, Either Hash Hash)])
--  deriving (Haskell.Show)

-- | Constructs a 'TreeProof' for membership or non-membership of an element in the tree.
--
-- Returns 'Nothing' if the element is invalid, indicating that it does not belong
-- to the set of strings 's' where the constraint is that `lower interval < s < upper upperd`.
-- It's using lexicographical order comparison.
--
-- The constraints (lower and upper interval) are specified by an initial empty tree, for example:
--
-- >>> fromList [Val "`" "{"] -- meaning only strings that starts with lowercase "a"-"z" are valid.
-- LabeledNode (Val {a = "`", b = "{"}) LabeledEmpty LabeledEmpty
--
-- > Note: Off-chain only
mkMembershipProof :: BuiltinByteString -> LabeledTree -> Maybe TreeProof
mkMembershipProof e = go []
  where
    go es = \case
      LabeledEmpty -> Nothing
      LabeledNode val l r
        | valid e val ->
            let nl = FullProof $ Node $ HashNode val (rootHash l) (rootHash r)
             in Just $ TreeProof (nl, es)
        | otherwise ->
            let vh = hashVal val
             in go ((vh, Right (rootHash r)) : es) l <|> go ((vh, Left (rootHash l)) : es) r

-- | Validates the membership status of an element using the root hash of the tree
-- and a provided 'TreeProof'.
-- The efficiency of the proof verification is logarithmic in the size of the tree.
--
-- > Note: Off- and On-chain
{-# INLINEABLE membership #-}
membership :: BuiltinByteString -> Hash -> Maybe TreeProof -> Bool
membership _ _ Nothing = False
membership
  e
  root
  (Just (TreeProof (FullProof (Node (HashNode val lh rh)), p))) =
    let vh = combineThreeHashes (hashVal val) lh rh
        -- Verifying a membership/non-membership 'TreeProof' for a given element in a LabeledTree
        -- involves combining hashes at each level. Triple hashes are used at every levels.
        go root' =
          \case
            [] -> root == root'
            (vh', Left l) : q -> go (combineThreeHashes vh' l root') q
            (vh', Right r) : q -> go (combineThreeHashes vh' root' r) q
     in valid e val && go vh p
membership _ _ _ = False -- Add a catch-all pattern for other cases

---------------------------------------------------------------------------------------------------
----------------------------------------- UPDATE PROOFS  ------------------------------------------

-- \| 'findAddUpdates' traverses a 'LabeledTree' to find nodes satisfying specific conditions.
--
--  | === Complexity:
--  * Time Complexity: O(log N) or O(N) in the worst case, where N is the number of nodes in the 'LabeledTree'.
--  * Space Complexity: O(2N) in the worst case, accounting for the existing tree and the list being built.
--
--  === Parameters:
--  * @root@: The root of the 'LabeledTree'.
--  * @e@: The target 'BuiltinByteString' to match.
--
--  === Returns:
--  A tuple of two 'LabeledTree' values representing the result of the traversal.
--
--  === Example:
--  @
--  let (updateNode, appendNode) = findNa tree "targetByteString"
--  @
--
-- > Note: Off-chain only
appendNodes :: LabeledTree -> BuiltinByteString -> (LabeledTree, LabeledTree)
appendNodes LabeledEmpty _ = (LabeledEmpty, LabeledEmpty)
appendNodes root e = go [root] (LabeledEmpty, LabeledEmpty)
  where
    go [] result = result
    go (current : rest) result@(nu, na) =
      case current of
        LabeledNode _ left right -> go (rest ++ [left, right]) (nnu, nna)
        LabeledEmpty -> go rest result
      where
        nnu = case (current, nu) of
          (LabeledNode val _ _, LabeledEmpty)
            | nonmember e val -> current
            | otherwise -> LabeledEmpty
          (_, _) -> nu

        nna = case (current, na) of
          (LabeledNode _ l r, LabeledEmpty)
            | l == LabeledEmpty || r == LabeledEmpty -> current
            | otherwise -> LabeledEmpty
          (_, LabeledNode {}) -> na
          (_, _) -> LabeledEmpty

-- | A constant representing the hash value of an empty string.
--
-- The 'emptyHash' value is computed once at runtime and can be used as a constant
-- throughout the program without recomputing the hash.
--
-- TODO: can plutus use it as non-inlinable code? Meaning refer to it as a constant instead
-- of inlining the function each time when it is called?
--
-- > Node: Both, off- and on-chain
emptyHash :: Hash
emptyHash = hash ""

{-
-- | Enforces specified rules for the given hash nodes.
--
-- The 'enforceRules' function takes three 'HashNode' parameters representing
-- the least common ancestor (lca), left, and right nodes, and checks whether
-- they satisfy the predefined rules. The rules are based on various conditions
-- involving 'Hash' and 'Val' types.
--
-- The function returns 'True' if the provided hash nodes adhere to the defined
-- rules, and 'False' otherwise.
--
-- Rules:
-- 1. lca is a hash, left is nu, right is na, na != nu, or
-- 2. lca is a hash, left is na, right is nu, na != nu, or
-- 3. lca is nu (nu != na), the left is a hash, right is na, hash != empty hash, or
-- 4. lca is nu (nu != na), the left is na, right is a hash, hash != empty hash, or
-- 5. lca is na (nu != na), left is nu, right is empty hash (na right is always an empty hash), or
-- 6. lca is na (and an nu as na == nu), left is an empty hash, the right is an empty hash, or
-- 7. lca is na (and an nu as na == nu), left is a hash, the right is an empty hash.
--
-- All the others are invalid.
--
-- > Note: the Na's index must be validated in the calling function.
--
-- Examples:
--
-- @
-- enforceRules x proofLca proofLeft proofRight -- Returns 'True' if the proofs are correct.
-- @
enforceRules :: BuiltinByteString -> HashNode -> HashNode -> HashNode -> Bool
enforceRules x lca left right =
  case (lca, left, right) of
    -- \| All rules assuming a valid x element for the corresponding rule.
    -- Rule 1 & 2 (LCA rules): Valid LCA, and Na, Nu or Nu, Na & Na != Nu
    (HashHash _, HashNode vl _ _, HashNode vr _ _) -> (nonmember x vl && (not $ nonmember x vr)) || ((not $ nonmember x vl) && nonmember x vr)
    -- Rule 3 (Nu rule): Valid Nu && H, Na && H != eH & Na != Nu
    (HashNode vlca _ _, HashHash h, HashNode n _ _) -> nonmember x vlca && (not $ nonmember x n) && h /= emptyHash
    -- Combined Rule 4 & 5
    -- Rule 4 (Nu rule): Valid Nu && Na, H && H != eH & Na != Nu
    -- Rule 5 (Na rule): Valid Na && Nu, H && H == eH & Na != Nu
    (HashNode vlca _ _, HashNode n _ _, HashHash h) -> (nonmember x vlca && (not $ nonmember x n) && h /= emptyHash) || ((not $ nonmember x vlca) && nonmember x n && h == emptyHash)
    -- Combined Rule 6 & 7
    -- Rule 6 (Nu/Na rule): Valid Na/Nu && H && eH && Na == Nu
    -- Rule 6 (Nu/Na rule): Valid Na/Nu && eH && eH && Na == Nu
    (HashNode vlca _ _, HashHash _, HashHash rh) -> nonmember x vlca && rh == emptyHash
    -- All other rules are invalid.
    _ -> False
-}

-- | **TreeProof Types**
--
-- A set of proof types that provide evidence and verification mechanisms for different operations on a labeled binary tree.
--
-- - __Node TreeProof:__
--   - Represents a signature for a tree node, consisting of the hash of the node's value (Val) and proofs for its left and right children.
--   - Example: `Hash( Hash(Val) || proof left || proof right )`.
--
-- - __Tree TreeProof (Root TreeProof):__
--   - Serves as a proof for the entire tree, starting from the root node.
--   - Comprises the node proof for the root node, including the hash of the root node's value and its left and right child proofs.
--
-- - __(Non)membership TreeProof:__
--   - Provides evidence for the presence or absence of an element in the tree.
--   - Based on a structure called 'TreeProof' containing a 'HashNode' (a compact representation of a labeled node) and an array of tuples.
--   - The tuples include the hash of a value and either the left or right hash for representing one of the two children's proofs.
--
-- - __Update (Append) TreeProof:__
--   - Proves the addition of an element to the root of the tree.
--   - Involves three proofs: 'pLca' (Lowest Common Ancestor to root), 'pL' (update node to Lowest Common Ancestor), and 'pR' (append node to Lowest Common Ancestor).
--   - Demonstrates that the newRootHash is derived from the old tree by adding an element, similar to Merkle trees but with some distinctions.
--
-- - __Update (Deletion) TreeProof:__
--   - Similar to the append proof but requires four proofs.
--   - Used to delete a node from the tree while maintaining its completeness.
--   - Involves updating the values of two nodes and removing a node in level order.
--
-- These proofs are used in @datag for proving (non)membership and tree updates...

-- | It returns a hasnode of a FullProof, otherwise trace error.
--
-- Use: Both, on-chain and off-chain
{-# INLINEABLE getNode #-}
getNode :: Proof -> HashNode
getNode (FullProof (Node n)) = n
getNode _ = traceError "wrong redeem"

-- | It returns a Val of a PartialProof, otherwise trace error.
--
-- Use: Both, on-chain and off-chain
{-# INLINEABLE getVal #-}
getVal :: Proof -> Val
getVal (PartialProof (ValProof v)) = v
getVal _ = traceError "wrong redeem"

-- | It returns a Hash either of the PartialProof or FullProof
--
-- Use: Both, on-chain and off-chain
{-# INLINEABLE getHash #-}
getHash :: Proof -> Maybe Hash
getHash (PartialProof (ValHash h)) = Just h
getHash (FullProof (NodeHash h)) = Just h
getHash _ = Nothing

-- It
-- Use: Both, on-chain and off-chain
{-# INLINEABLE isNu #-}
isNu :: BuiltinByteString -> Val -> Bool
isNu e (Val _ a b) = a < e && e < b

{-# INLINEABLE getNu #-}
getNu :: BuiltinByteString -> Proof -> Maybe Val
getNu e (PartialProof (ValProof v@(Val _ a b)))
  | a < e && e < b = Just v
  | otherwise = Nothing
getNu e (FullProof (Node (HashNode v@(Val _ a b) _ _)))
  | a < e && e < b = Just v
  | otherwise = Nothing
getNu _ _ = Nothing

{-# INLINEABLE getUpdatedNodes #-}
getUpdatedNodes :: BuiltinByteString -> Val -> Integer -> (Val, Val)
getUpdatedNodes x (Val i a b) leafIdx =
  if a < x && x < b
    then (nu, leaf)
    else traceError "invalid redeemer"
  where
    nu = Val i a x
    leaf = Val leafIdx x b

{-updateNa :: BuiltinByteString -> Proof -> Proof -> (Proof, Proof)
updateNa e (FullProof (Node fp1)) (FullProof (Node fp2)) = (lp', rp')
updateNa e pp (FullProof (Node fp)) = (lp', rp')
  where
    v = getVal pp

    ii = isNu e v

updateNa _ _ _ = traceError "invalid redeemer"

-}

-- | the `validateNodes` validates the proofs nodes if there is any.
--
-- It returns the updated lca, nl and nr based on the valid combinations:
-- H,N,N
-- 1.  (lca, lcaps), (nu, nlps),  (na, nrps)
-- 2.  (lca, lcaps), (na, nlps),  (nu, nrps)
-- N,N,H
-- 3.  ( nu, lcaps), (na, nlps),  ( h, nrps)
-- 4.  ( na, lcaps), (nu, nlps),  (eh, nrps)
-- N,H,N
-- 5.  ( nu, lcaps), ( h, nlps),  (na, nrps)
-- N,H,H
-- 6.  (nau, lcaps), (eh, nlps),  (eh, nrps)
-- 7.  (nau, lcaps), ( h, nlps),  (eh, nrps)
--
-- Note: The assumption is that the element validity has been already checked previously.
-- It's very important that only a valid element (xa < x < xb) is passed to this function.
--
-- ** Cabal Repl Example **
-- >>> :l LabeledTree
-- >>> :set -XOverloadedStrings
-- >>> let eh = FullProof $ NodeHash emptyHash
-- >>> let h = FullProof $ NodeHash $ hash "1234"
-- >>> let ph = TreeProof (h, [])
-- >>> let pe = TreeProof (eh, [])
--
-- >>> let na  = FullProof $ Node $ HashNode (Val  1 "irma"  "{" ) emptyHash emptyHash
-- >>> let pna = TreeProof (na, [])
--
-- >>> let nu  = PartialProof $ ValProof (Val  1 "`"  "irma" )
-- >>> let pnu = TreeProof (nu, [])
--
-- >>> validateNodes "alma" pnu  ph pna
-- Just (PartialProof (ValProof (Val {xi = 1, xa = "`", xb = "alma"})),FullProof (NodeHash 03ac67),FullProof (Node (HashNode (Val {xi = 1, xa = "irma", xb = "{"}) 4804c9 e3b0c4)))
{-# INLINEABLE validateNodes #-}
validateNodes :: BuiltinByteString -> TreeProof -> TreeProof -> TreeProof -> Maybe (Proof, Proof, Proof)
validateNodes e (TreeProof (nlca, _)) (TreeProof (nl, _)) (TreeProof (nr, _)) =
  case (getHash nlca, getHash nl, getHash nr) of
    -- Combination 1, 2
    -- ValHash, Na, Na or ValHash, Nu, Na
    -- Conditions:
    --  1. Val(Na)!=Val(Nu)
    --  2. One of them is Nu (a<x<b) the other is Na (if FullProof then right Na == emptyHash)
    -- TODO: Draft
    (Just _, Nothing, Nothing)
      | nuv == nav -> traceError "invalid redeemer"
      | nar /= emptyHash -> traceError "invalid redeemer"
      | otherwise -> Just (nlca', nl', nr')
      where
        nlca' = nlca

        nll@(HashNode lv _ _) = getNode nl
        nrr = getNode nr

        (HashNode nuv nul nur, HashNode nav nal nar) = if isNu e lv then (nll, nrr) else (nrr, nll)

        Val nai _ _ = nav

        eh = nal == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1

        (nu', leaf) = getUpdatedNodes e nuv i

        pleaf = combineThreeHashes (hashVal leaf) emptyHash emptyHash

        (nal', nar') = if eh then (pleaf, emptyHash) else (nal, pleaf)
        n1 = FullProof $ Node $ HashNode nav nal' nar'

        n2 = FullProof $ Node $ HashNode nu' nul nur

        (nl', nr') = if isNu e lv then (n2, n1) else (n1, n2)

    -- Combination 3, 4
    -- Na, Nu, NodeHash, or Nu, Na, NodeHash
    -- Conditions: Na != Nu
    -- when lca == na then NodeHash must be == emptyHash
    -- when lca == nu then NodeHash must be /= emtpyHash and left na == emptyHash
    -- TODO: Progressing
    (Nothing, Nothing, Just hr)
      -- | lcav == plv -> traceError "invalid redeemer"
      -- \| v when lca == na, then the hr must be empty hash. This is a cheap evaluation.
      -- | (not $ isNu e lcav) && (hr == emptyHash) -> traceError "invalid redeemer"
      -- \| v when lca == na, then the hr must be empty hash. This is a cheap evaluation.
      -- | isNu e lcav && (hr == emptyHash) -> traceError "invalid redeemer"
      | otherwise -> Just (nlca', nl', nr')
      where
        lcav = getVal nlca
        -- plv = getVal nl

        -- nr' = if isNu e lcav then FullProof $ NodeHash emptyHash else nr
        
        (nlca', nl', nr') = case isNu e lcav of
              True -> do
                let HashNode nav nal nar = getNode nl
                    Val nai _ _ = nav
                    eh = nal == emptyHash
                    i = if eh then 2 * nai else 2 * nai + 1
                    (nu', leaf) = getUpdatedNodes e lcav i

                    leafh = combineThreeHashes (hashVal leaf) emptyHash emptyHash

                    (lh, rh) = if eh then (leafh, nar) else (nal, leafh)
                    
                    na = FullProof  $ NodeHash $ combineThreeHashes (hashVal nav) lh rh

                    nu = PartialProof $ ValProof nu'

                (nu, na, nr)

              False -> do
                let Val nai _ _  = lcav

                    i = 2 * nai + 1
                    (nu', leaf) = getUpdatedNodes e lcav i

                    pleaf = combineThreeHashes (hashVal leaf) emptyHash emptyHash

                    nar = FullProof $ NodeHash pleaf
                    
                    HashNode _ nul nur = getNode nl
      
                    nal = FullProof (Node $ HashNode nu' nul nur)
                (nlca, nal, nar)

    (Nothing, Just hl, Nothing)
      -- Combination 5
      -- Nu,H, Na
      -- Invalid when lca == na or nr is an empty hash to enforce completeness.
      -- These cannot be happen as these would not be able to recreate the old state's hash anyway.
      -- TODO: Draft
      | nuv == nav -> traceError "invalid redeemer"
      | not $ isNu e nuv -> traceError "invalid redeemer"
      | hl == emptyHash -> traceError "invalid redeemer"
      | nar /= emptyHash -> traceError "invalid redeemer"
      | otherwise -> Just (nlca', nl, nr')
      where
        -- nlca must be the Nu
        nuv = getVal nlca
        -- nr must be the Na
        HashNode nav@(Val nai _ _) nal nar = getNode nr

        eh = nal == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1
        (nu', leaf) = getUpdatedNodes e nuv i

        pleaf = combineThreeHashes (hashVal leaf) emptyHash emptyHash

        (nal', nar') = if eh then (pleaf, nar) else (nal, pleaf)
        nr' = FullProof $ Node $ HashNode nav nal' nar'
        nlca' = PartialProof $ ValProof nu'

    -- \| Combination 6,7, where Nu == Na
    -- Ivalid when hr /= emptystring, hl can be eithger.
    -- TODO: Draft
    (Nothing, Just hl, Just hr)
      | hr /= emptyHash -> traceError "invalid redeemer"
      | not $ isNu e nuv -> traceError "invalid redeemer"
      | otherwise -> Just (nlca', nl', nr')
      where
        -- Nu == Na
        nuv@(Val nai _ _) = getVal nlca

        eh = hl == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1
        (nu, leaf) = getUpdatedNodes e nuv i

        nleaf = FullProof (Node $ HashNode leaf emptyHash emptyHash)
        nlca' = PartialProof (ValProof nu)

        (nl', nr') = if eh then (nleaf, nr) else (nl, nleaf)
    _ -> Nothing
{-
{-# INLINEABLE validateNodes2 #-}
validateNodes2 :: BuiltinByteString -> TreeProof -> TreeProof -> TreeProof -> Maybe (Proof, Proof, Proof)
validateNodes2 e (TreeProof (nlca, _)) (TreeProof (nl, _)) (TreeProof (nr, _)) =
  case (getHash nlca, getHash nl, getHash nr) of
    (Just _, Nothing, Nothing) -> combination1and2 e nlca nl nr
    (Nothing, Nothing, Just hr) -> combination3and4 e nlca hr nl nr
    (Nothing, Just hl, Nothing) -> combination5 e nlca hl nr
    (Nothing, Just hl, Just hr)  -> combination6and7 e nlca hl hr nl nr
    _ -> Nothing
  where
    combination1and2 e nlca nl nr
      | nuv == nav && nar == emptyHash = Just (nlca', nl', nr')
      | otherwise = traceError "invalid redeemer"
      where
        nlca' = nlca
        nll@(HashNode lv _ _) = getNode nl
        nrr = getNode nr
        (HashNode nuv nul nur, HashNode nav nal nar) =
          if isNu e lv then (nll, nrr) else (nrr, nll)

        Val nai _ _ = nav
        eh = nal == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1
        (nu', leaf) = getUpdatedNodes e nuv i
        pleaf = combineThreeHashes (hashVal leaf) emptyHash emptyHash
        (nal', nar') = if eh then (pleaf, emptyHash) else (nal, pleaf)
        n1 = FullProof $ Node $ HashNode nav nal' nar'
        n2 = FullProof $ Node $ HashNode nu' nul nur
        (nl', nr') = if isNu e lv then (n2, n1) else (n1, n2)

    combination3and4 e nlca hr nl nr
      | lcav == hr && hr == emptyHash = Just (nlca', nl', nr')
      | not (isNu e lcav) && hr == emptyHash = Just (nlca', nl, nr)
      | isNu e lcav && hr == emptyHash = Just (nlca', nl, nr)
      | otherwise = traceError "invalid redeemer"
      where
        lcav = getVal nlca
        (nlca', nl', nr') =
          if isNu e lcav
            then (PartialProof $ ValProof nu', FullProof na, nr)
            else (nlca, FullProof nal, FullProof nar)
        (nu', leaf) = getUpdatedNodes e lcav (if isNu e lcav then 2 * nai else 2 * nai + 1)
        leafh = combineThreeHashes (hashVal leaf) emptyHash emptyHash
        (lh, rh) = if eh then (leafh, nar) else (nal, leafh)
        na = NodeHash $ FullProof $ Node $ HashNode nav lh rh

    combination5 e nlca hl nr
      | nuv == nav && isNu e nuv && hl /= emptyHash && nar == emptyHash = Just (nlca', nl, nr')
      | otherwise = traceError "invalid redeemer"
      where
        nuv = getVal nlca
        HashNode nav@(Val nai _ _) nal nar = getNode nr
        eh = nal == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1
        (nu', leaf) = getUpdatedNodes e nuv i
        pleaf = combineThreeHashes (hashVal leaf) emptyHash emptyHash
        (nal', nar') = if eh then (pleaf, nar) else (nal, pleaf)
        nr' = FullProof $ Node $ HashNode nav nal' nar'
        nlca' = PartialProof $ ValProof nu'

    combination6and7 e nlca hl hr nl nr
      | nuv == nav && hr == emptyHash && isNu e nuv = Just (nlca', nl', nr')
      | otherwise = traceError "invalid redeemer"
      where
        nuv@(Val nai _ _) = getVal nlca
        eh = hl == emptyHash
        i = if eh then 2 * nai else 2 * nai + 1
        (nu, leaf) = getUpdatedNodes e nuv i
        nleaf = FullProof (Node $ HashNode leaf emptyHash emptyHash)
        nlca' = PartialProof (ValProof nu)
        (nl', nr') = if eh then (nleaf, nr) else (nl, nleaf)
-}
-- validateVals :: Val -> Val

{-
-- Plutus
  (nlca, nl, nr) = validateNodes x TreeProof (pLca pL pR

  -- These are the most time and space expensive validations.
  -- verify the old tree's root hash
  -- The worst case is 4*O(logN)
  polh = rootHash nl lps
  porh = rootHash nr rps
  poh = rootHash (lca polh porh) lcaps

  -- verify the old tree's root hash
  pnlh = rootHash nl' lps
  pnrh = rootHash nr' rps
  pnh  = rootHash (lca' pnlh pnrh) lcaps

  oh == poh && nh == pnh

-}

-- TemplateHaskell
PlutusTx.unstableMakeIsData ''HashNode
PlutusTx.unstableMakeIsData ''ValProof
PlutusTx.unstableMakeIsData ''NodeProof
PlutusTx.unstableMakeIsData ''Proof
PlutusTx.unstableMakeIsData ''ProofType
PlutusTx.unstableMakeIsData ''TreeProof