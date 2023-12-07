{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Maybe (fromJust, fromMaybe)
import qualified Debug.Trace
import qualified GHC.Generics as LabeledTree
import LabeledTree
import PlutusLedgerApi.V2
import PlutusPrelude ((<|>))
import PlutusTx.Maybe (isJust)
import qualified PlutusTx.Prelude
import System.IO (IO, print)
import Prelude
import qualified Prelude as Haskell

append :: LabeledTree -> BuiltinByteString -> LabeledTree -> LabeledTree -> LabeledTree
append LabeledEmpty _ _ _ = LabeledEmpty
append _ _ LabeledEmpty _ = LabeledEmpty
append _ _ _ LabeledEmpty = LabeledEmpty
append tree x (LabeledNode vnu _ _) (LabeledNode vna lna rna)
  -- v the x must not be a member of the tree i.e. there must be a node with Val with `a < x < b`
  -- and the update node must have an empty right
  | nonmember x vnu && rna == LabeledEmpty = go tree
  | otherwise = LabeledEmpty
  where
    -- the element is a valid non member of the tree
    -- modify the update node's Val to a x
    (Val i a b) = vnu
    nvnu = Val i a x

    (Val xi' _ _) = vna
    -- Append the new child to the leaf (append node)
    ni = if lna == LabeledEmpty then 2 * xi' else 2 * i + 1
    child = LabeledNode (Val ni x b) LabeledEmpty LabeledEmpty
    leaf = case lna of
      LabeledEmpty -> LabeledNode vna child rna
      _ -> LabeledNode vna lna child

    go LabeledEmpty = LabeledEmpty
    go (LabeledNode val left right)
      | val == vnu = LabeledNode nvnu (go left) (go right)
      | val == vna = leaf
      | otherwise = LabeledNode val (go left) (go right)

-- | `mkProof` generates a proof (`rootHash`) of a tree (or branch) starting from a given node.
--
-- It generates a proof from any node to its ancestor within the tree.
--
-- Note: To create a valid proof, the root must be the ancestor of the given node. Otherwise, it returns `Nothing`.
--
-- The function takes the following parameters:
--
--   * `node`: The node for which the proof is generated.
--   * `ancestor`: The ancestor of the node (the root node is ancestor of every nodes of the tree).
--   * `proofType`: The type of proof to generate (`Partial` or `Full`).
--   * `asHash`: A boolean indicating whether the initial value of the generated proof should be a hash (`ValHash` or `NodeHash`)
--     or a type (`Val`, `HashNode`).
--
-- It returns a proof when the target node is the ancestor of the given node, otherwise `Nothing`.
mkProof :: LabeledTree -> LabeledTree -> ProofType -> Bool -> Maybe TreeProof
mkProof LabeledEmpty _ _ _ = Nothing
mkProof _ LabeledEmpty _ _ = Nothing
mkProof (LabeledNode val _ _) ancestor proofType asHash = go [] ancestor
  where
    go es = \case
      LabeledEmpty -> Nothing
      LabeledNode v l r
        | val == v ->
            let nl = case proofType of
                  Partial -> PartialProof $ if asHash then ValHash (hashVal val) else ValProof val
                  Full -> FullProof $ if asHash then NodeHash (rootHash ancestor) else Node $ HashNode val (rootHash l) (rootHash r)
             in Just $ TreeProof (nl, es)
        | otherwise ->
            let vh = hashVal v
             in go ((vh, Right (rootHash r)) : es) l <|> go ((vh, Left (rootHash l)) : es) r

-- Assuming the definitions of `rootLabel`, `hashVal`, and `rootHash` are available.

clone :: LabeledTree -> LabeledTree
clone LabeledEmpty = LabeledEmpty
clone node@(LabeledNode val l r) = LabeledNode val (clone l) (clone r)

-- | `mkUpdateProof` generates a proof for updating a tree.
--
-- This proof guarantees that the updated tree results from the old tree with an added or removed element.
--
-- For instance, the equality `hashRoot newTree == hashRoot (oldTree +/− element)` holds,
-- indicating that the hash root of the updated tree matches the hash root of the old tree with an added or removed element.
--
-- > Note: When updating the tree, it's essential to identify the correct node for appending after the last element
-- or removing the last element, ensuring the tree's completeness is maintained.
--
-- An index in the node's value is used to enforce the correct position for insertion or removal.
-- This measure safeguards against adversaries attempting to create a non-complete binary tree during tree updates.
--
-- The steps for generating the proof are as follows:
--
-- 1. Identify the update nodes based on the add or delete/remove operation:
--    - For inserting an element into the tree, find the update and append nodes.
--    - For removing an element from the tree, find update1, update2, and delete nodes.
--
-- 2. Find the Lowest Common Ancestor (LCA) for two nodes (append) or three nodes (deletion).
--
-- 3. Construct three proofs (append) or four proofs (removal) based on the LCA:
--    * For insertion:
--       - TreeProof from LCA to the root.
--       - TreeProof from the update node to the LCA.
--       - TreeProof from the append node to the LCA.
--    - For removal:
--       - TreeProof from LCA to the root.
--       - TreeProof from update1 node to the LCA.
--       - TreeProof from update2 node to the LCA.
--       - TreeProof from delete node to the LCA.
-- 4. Pass the proof as redeemer to the corresponding Plutus minting policy.
mkUpdateProof :: LabeledTree -> BuiltinByteString -> Maybe Bool
mkUpdateProof LabeledEmpty _ = Nothing
mkUpdateProof root e = Nothing

verifyProof :: TreeProof -> TreeProof -> TreeProof -> Hash
verifyProof plca pl pr = emptyHash

nodes :: BuiltinByteString -> (TreeProof, TreeProof, TreeProof) -> Maybe (Val, Val)
nodes e (TreeProof (PartialProof vlca, _), TreeProof (FullProof vl, _), TreeProof (FullProof vr, _)) =
  case (vlca, vl, vr) of
    (ValProof v, Node (HashNode v' _ _), _) -> go v v'
    (ValProof v, _, Node (HashNode v' _ _)) -> go v v'
    (_, Node (HashNode v _ _), Node (HashNode v' _ _)) -> go v' v
    (_, _, _) -> Nothing
  where
    go :: Val -> Val -> Maybe (Val, Val)
    go v1 v2
      | nonmember e v1 && not (nonmember e v2) = Just (v1, v2)
      | nonmember e v2 && not (nonmember e v1) = Just (v2, v1)
      | otherwise = Nothing
nodes _ _ = Nothing

verifyUpdate :: BuiltinByteString -> Integer -> Hash -> Hash -> (TreeProof, TreeProof, TreeProof) -> Bool
verifyUpdate x size os ns proofs@(pLca, pL, pR) = do
  let TreeProof (lcav, lcap) = pLca
      TreeProof (lv, lp) = pL
      TreeProof (rv, rp) = pR

      -- Get the update- and append nodes
      (nu, na) = fromJust $ nodes x proofs
      -- Get the Val's properties of the update node `nu`
      (Val i a b) = nu

      -- Update the update node from Val (idx xa xb) to Val (idx xa x) and
      -- create the leaf the new child of the append node. Val (size, x, xb)
      nu' = Val i a x
      val = Val size x b

      -- the Na's child is constructed from Nu -> Val (idx xa xb)
      leaf = rootHash $ LabeledNode val LabeledEmpty LabeledEmpty

      proofs' = (TreeProof (lcav, lcap), TreeProof (lv, lp), TreeProof (rv, rp))
  -- verify the previous state's proof
  -- proof  = verifyProof pLca pL pR -- (lca, lcap) (lv, lp) (rv, rp)
  -- proof' = verifyProof (nlcav, nlcap) (nlv, nlp) (nrv, nrp)
  -- plca = case lcav of
  --  HashHash h -> HashHash $ combineThreeHashes h (updateCheck pL) (updateCheck pR)
  --  HashNode v _ _ -> HashHash $ combineThreeHashes (hashVal v) (updateCheck pL) (updateCheck pR)
  -- oos = updateCheck $ TreeProof (plca, lcap)
  os == os -- oos

-- True
-- True -- os /= ns && os == proof && ns == proof'

buildProof :: TreeProof -> TreeProof -> TreeProof -> Hash
buildProof plca pl pr =
  emptyHash

updateCheck :: TreeProof -> Maybe Hash
updateCheck (TreeProof (PartialProof _, _)) = Nothing
updateCheck (TreeProof (FullProof h, p)) =
  let vh = case h of
        (NodeHash h) -> h
        Node (HashNode val l r) -> combineThreeHashes (hashVal val) l r
      go root' =
        \case
          [] -> Just root'
          (vh', Left l) : q -> go (combineThreeHashes vh' l root') q
          (vh', Right r) : q -> go (combineThreeHashes vh' root' r) q
   in go vh p

-- | mkAppendProof creates proof for tree update (append element into the tree)
--
-- Invalid combinations are no threats as they would generate different hashRoot for the
-- the tree to update, meaning rootHash tree /= verifyProof tree proofs
--
-- >>> rootHash tree == verifyProofs proofs
-- True
-- >>> rootHash appendedTree == verifyProofs proofs'
-- True
mkAppendProof :: LabeledTree -> BuiltinByteString -> Maybe (TreeProof, TreeProof, TreeProof)
mkAppendProof LabeledEmpty _ = Nothing
mkAppendProof tree e = do
  let (nu, na) = appendNodes tree e
      lca = findLca tree nu na
  if nu == LabeledEmpty || na == LabeledEmpty || lca == LabeledEmpty
    then Nothing
    else -- All of them is a LabeledNode from now own.

      if nu == na
        then -- lca == nu == na FIXME: in verification we must ansure this canstraint i.e. lca == nu == na
        -- as nu == na check is not enough.

          let LabeledNode _ ul ur = nu
              plca = fromJust $ mkProof nu tree Partial False
              pl = TreeProof (FullProof (NodeHash $ rootHash ul), []) -- Not calling mkProof but manually constructing
              pr = TreeProof (FullProof (NodeHash $ rootHash ur), []) -- Not calling mkProof but manually constructing
           in Just (plca, pl, pr)
        else -- nu parent of na

          if lca == nu
            then do
              let LabeledNode uv ul ur = nu
                  -- LabeledNode av al ar = na

                  plca = fromJust $ mkProof nu tree Partial False
                  tp@(TreeProof (_, p)) = fromJust $ mkProof na nu Full False
                  pp@(h, ei) = last p
                  (pL, pR) = case ei of
                    Left _ -> (TreeProof (FullProof (NodeHash $ rootHash ul), []), tp)
                    Right _ -> (tp, TreeProof (FullProof (NodeHash $ rootHash ur), []))
              Just (plca, pL, pR)
            else -- na parent of nu

              if lca == na
                then do
                  let LabeledNode av al ar = na

                      plca = fromJust $ mkProof na tree Partial False
                      tp@(TreeProof (_, p)) = fromJust $ mkProof nu na Full False
                      (_, ei) = last p
                      (pL, pR) = case ei of
                        Left _ -> (TreeProof (FullProof (NodeHash $ rootHash al), []), tp)
                        Right _ -> (tp, TreeProof (FullProof (NodeHash $ rootHash ar), []))
                  Just (plca, pL, pR)
                else -- no any of the nodes is parent of the other
                do
                  let plca = fromJust $ mkProof lca tree Partial True

                      tu = fromJust $ mkProof nu lca Full False
                      ta@(TreeProof (_, p)) = fromJust $ mkProof na lca Full False
                      (_, ei) = last p
                      (pL, pR) = case ei of
                        Left _ -> (tu, ta)
                        Right _ -> (ta, tu)
                  Just (plca, pL, pR)

-------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------
-- Your actual values
val1 = Val 1 "`" "c"

val2 = Val 2 "z" "{"

val3 = Val 3 "f" "g"

val4 = Val 4 "j" "l"

val5 = Val 5 "d" "e"

val6 = Val 6 "y" "z"

val7 = Val 7 "g" "h"

val8 = Val 8 "l" "o"

val9 = Val 9 "u" "ul"

val10 = Val 10 "s" "u"

val11 = Val 11 "x" "y"

val12 = Val 12 "e" "f"

val13 = Val 13 "o" "s"

val14 = Val 14 "i" "j"

val15 = Val 15 "v" "x"

val16 = Val 16 "c" "d"

val17 = Val 17 "h" "i"

val18 = Val 18 "ul" "v"

val19 = Val 19 "ubul" "ul"

nval9 = Val 9 "u" "ubul"

eh :: LabelTree
eh = LabelHash $ hash ""

labelTree :: LabelTree
labelTree =
  LabelNode
    val1
    ( LabelNode
        val2
        ( LabelNode
            val4
            ( LabelNode
                val8
                (LabelNode val16 eh eh)
                (LabelNode val17 eh eh)
            )
            ( LabelNode
                val9
                (LabelNode val18 eh eh)
                eh
            )
        )
        ( LabelNode
            val5
            (LabelNode val10 eh eh)
            (LabelNode val11 eh eh)
        )
    )
    ( LabelNode
        val3
        ( LabelNode
            val6
            (LabelNode val12 eh eh)
            (LabelNode val13 eh eh)
        )
        ( LabelNode
            val7
            (LabelNode val14 eh eh)
            (LabelNode val15 eh eh)
        )
    )

labelTree2 :: LabelTree
labelTree2 =
  LabelNode
    val1
    ( LabelNode
        val2
        ( LabelNode
            val4
            ( LabelNode
                val8
                (LabelNode val16 eh eh)
                (LabelNode val17 eh eh)
            )
            ( LabelNode
                nval9
                (LabelNode val18 eh eh)
                (LabelNode val19 eh eh)
            )
        )
        ( LabelNode
            val5
            (LabelNode val10 eh eh)
            (LabelNode val11 eh eh)
        )
    )
    ( LabelNode
        val3
        ( LabelNode
            val6
            (LabelNode val12 eh eh)
            (LabelNode val13 eh eh)
        )
        ( LabelNode
            val7
            (LabelNode val14 eh eh)
            (LabelNode val15 eh eh)
        )
    )

data LabelTree
  = LabelHash Hash
  | LabelNode Val LabelTree LabelTree
  deriving (Show)

rootHash' :: LabelTree -> Hash
rootHash' tree = case tree of
  LabelHash h -> h
  LabelNode v l r -> combineThreeHashes (hashVal v) (rootHash' l) (rootHash' r)

updateNodes :: BuiltinByteString -> Val -> Val -> Integer -> (Val, Val, Val)
updateNodes x nu@(Val i a b) na idx =
  if a < x && x < b
    then (nu', na', leaf)
    else PlutusTx.Prelude.traceError "invalid redeemer"
  where
    nu' = Val i a x
    -- Clone it
    na' = if nu == na then nu else na
    leaf = Val idx x b

data Proving = OldState | NewState
  deriving Show

{-# INLINEABLE checkAppend #-}
checkAppend :: BuiltinByteString -> Integer -> Val -> Val -> Proving -> Hash -> LabelTree -> Bool
checkAppend e tsize nuv nav state root proof = hasNu && hasNa && root' == root
  where
    nuHash = hashVal nuv
    naHash = hashVal nav
    (nuv', nav', leaf) = updateNodes e nuv nav tsize

    go hasNu' hasNa' =
      \case
        LabelHash h -> (hasNu', hasNa', h)
        LabelNode v l r ->
          let vh = hashVal v
              hasNu'' = hasNu' || vh == nuHash
              hasNa'' = hasNa' || vh == naHash

              (lnu, lna, lh) = go hasNu'' hasNa'' l
              (rnu, rna, rh) = go hasNu'' hasNa'' r

              isAny = vh == nuHash || vh == naHash
              (vh', lh', rh') = case (state, isAny) of
                (NewState, True) ->
                  let leafh = combineThreeHashes (hashVal leaf) emptyHash emptyHash
                      newNuHash = if vh == nuHash then hashVal nuv' else hashVal nav'
                      (nlh, nrh) = if lh == emptyHash then (leafh, rh) else (lh, leafh)
                   in (newNuHash, nlh, nrh)
                (_, _) -> (vh, lh, rh)
           in (lnu || rnu, lna || rna, combineThreeHashes vh' lh' rh')

    (hasNu, hasNa, root') = go False False proof



main :: IO ()
main = do
  let oth = rootHash' labelTree
      nth = rootHash' labelTree2
      leaf = Val 19 "ubul" "ul"
  print $ checkAppend "ubul" 19 val9 val9 OldState oth labelTree
  print $ checkAppend "ubul" 19 val9 val9 NewState nth labelTree


-- print hashVal

-- print  rh
-- print $ membership2 val12 rh labelTree
{-
  let eh = FullProof $ NodeHash emptyHash
  let h = FullProof $ NodeHash $ hash "1234"
  let ph = TreeProof (h, [])
  let pe = TreeProof (eh, [])

  let na  = FullProof $ Node $ HashNode (Val  2 "irma"  "{" ) emptyHash emptyHash
  let pna = TreeProof (na, [])

  let na1  = FullProof $ Node $ HashNode (Val  2 "irma"  "{" ) (hash "1234") emptyHash
  let pna1 = TreeProof (na1, [])

  let na2  = FullProof $ Node $ HashNode (Val  2 "irma"  "{" ) emptyHash (hash "1234")
  let pna2 = TreeProof (na2, [])

  let nu  = FullProof $ Node $ HashNode  (Val  1 "`"  "irma" ) (hash "1234") (hash "2345")
  let pnu = TreeProof (nu, [])

  let a = validateNodes "alma"  pna pnu pe
  print a
  let b = validateNodes "alma"  pnu pna pe
  print b
  let c = validateNodes "alma"  pnu pna ph
  print c
  let d = validateNodes "alma"  pna pnu pe

  print d

  --- ####### Check proofs....
-}
{- let x = "joska" -- Nu, Nu, Na 4-4-9  Nu parent Na
  -- let x = "dodo"  -- Vh, Nu, Na H-5-9  Na & Nu on different branch
  -- let x = "ulan"  -- Na, Nu, Na 9-18-? Na parent Nu
  -- let x = "ubul" -- Na, Na, Na 9-9-9  Na==Nu
      ot = LabeledTree.fromList sampleVals
      (nu, na) = appendNodes ot x
      ut = append ot x nu na
      lca = findLca ot nu na
      Just (plca@(TreeProof (i1, p1)), pl@(TreeProof (i2, p2)),pr@(TreeProof(i3,p3))) = mkAppendProof ot x

  putStrLn "########## Updated Tree #######"
  printTree ot
  putStrLn "########## LCA #######"
  print $ hashNode lca
  putStrLn "########## pLCA, pL pR #######"
  print plca
  print pl
  print pr
  putStrLn "########## Updated Tree #######"
  printTree ut
  putStrLn "########## Verify Old Proofs #######"
  let TreeProof (PartialProof (ValProof v), hps) = plca
      lh = fromJust $ updateCheck pl
      rh = fromJust $ updateCheck pr
      ntp = TreeProof (
        FullProof (Node (HashNode v lh rh)) ,
        hps
        )

  let oh = fromJust $ updateCheck ntp
  putStrLn $ "Original  : " ++ Haskell.show ( rootHash ot )
  putStrLn $ "Calculated: " ++ Haskell.show oh

  putStrLn "########## Verify New Proofs #######"
  -- Build the new Val and the leaf to append.
  -- print $ rootHash ut

-}
{-  let x = "joska"
      oldtree = LabeledTree.fromList sampleVals
      -- oldtree = sampleTree
      (nu, na) = appendNodes oldtree x
      updated = append oldtree x nu na
  -- proof = mkAppendProof x nu na updated

  printTree oldtree

  printTree updated
  putStrLn $ "NU : " ++ (show $ hashNode nu)
  putStrLn $ "NA : " ++ (show $ hashNode na)

  let xx = lca oldtree nu na

  putStrLn $ "LCA: " ++ (show $ hashNode xx)
-}

{- Enforce rules....
  let eh = HashHash emptyHash
      sh = HashHash $ hash "2"
      v4 = HashNode (Val 4 "j" "l") emptyHash emptyHash
      v2 = HashNode (Val 2 "z" "{") emptyHash emptyHash
      v5 = HashNode (Val 5 "d" "e") emptyHash emptyHash
      v8 = HashNode (Val 8 "l" "o") emptyHash emptyHash
      v9 = HashNode (Val 9 "u" "ul") emptyHash emptyHash
      v10 = HashNode (Val 10 "s" "u") emptyHash emptyHash
      v18 = HashNode (Val 18 "ul" "v") emptyHash emptyHash

  -- LCA rules
  putStrLn "### LCA rules"
  print $ enforceRules "lali" sh v8 v9 -- Rule 1
  print $ enforceRules "sio" sh v9 v10 -- Rule 2

  -- Nu rules
  putStrLn "### Nu rules"
  print $ enforceRules "joska" v4 sh v9
  print $ not $ enforceRules "joska" v4 eh v9 -- Should fail
  print $ enforceRules "dodo" v5 v10 sh
  print $ not $ enforceRules "dodo" v5 v10 eh -- Should fail
  putStrLn "### Na rules"
  print $ enforceRules "ulan" v9 v18 eh
  print $ not $ enforceRules "ulan" v9 v18 sh -- Should fail
  putStrLn "### Na==Nu rules"
  print $ enforceRules "ubul" v9 sh eh
  print $ not $ enforceRules "ubul" v9 sh sh -- Should fail
  print $ enforceRules "ubul" v9 eh eh
  print $ not $ enforceRules "ubul" v9 eh sh -- Should fail
  print $ not $ enforceRules "ubul" v9 sh v2 -- Should fail but succeeding
  print $ not $ enforceRules "ubul" v9 v2 sh -- Should fail but succeeding
-}