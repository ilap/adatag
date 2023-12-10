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
import System.IO (IO, print)
-- import Prelude
import qualified Prelude as Haskell

import Prelude (Int, String, otherwise, (^),  (-), ($))
-- import qualified PlutusTx.Builtins.Class
import qualified PlutusTx.Prelude
import PlutusTx
import PlutusTx.Prelude
import qualified GHC.TypeLits as PlutusTx.Builtins

{-
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
-}
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
{-mkProof :: LabeledTree -> LabeledTree -> ProofType -> Bool -> Maybe TreeProof
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
-}
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

eh :: ProofTree
eh = ProofLeaf Prelude.$ hash ""

labelTree :: ProofTree
labelTree = ProofNode
    val1
    ( ProofNode
        val2
        ( ProofNode
            val4
            ( ProofNode
                val8
                (ProofNode val16 eh eh)
                (ProofNode val17 eh eh)
            )
            ( ProofNode
                val9
                (ProofNode val18 eh eh)
                eh
            )
        )
        ( ProofNode
            val5
            (ProofNode val10 eh eh)
            (ProofNode val11 eh eh)
        )
    )
    ( ProofNode
        val3
        ( ProofNode
            val6
            (ProofNode val12 eh eh)
            (ProofNode val13 eh eh)
        )
        ( ProofNode
            val7
            (ProofNode val14 eh eh)
            (ProofNode val15 eh eh)
        )
    )

labelTree2 :: ProofTree
labelTree2 =
  ProofNode
    val1
    ( ProofNode
        val2
        ( ProofNode
            val4
            ( ProofNode
                val8
                (ProofNode val16 eh eh)
                (ProofNode val17 eh eh)
            )
            ( ProofNode
                nval9
                (ProofNode val18 eh eh)
                (ProofNode val19 eh eh)
            )
        )
        ( ProofNode
            val5
            (ProofNode val10 eh eh)
            (ProofNode val11 eh eh)
        )
    )
    ( ProofNode
        val3
        ( ProofNode
            val6
            (ProofNode val12 eh eh)
            (ProofNode val13 eh eh)
        )
        ( ProofNode
            val7
            (ProofNode val14 eh eh)
            (ProofNode val15 eh eh)
        )
    )


-- i2i :: Int -> Integer
-- i2i = toBuiltin

-- s2Bs :: String -> BuiltinByteString
-- s2Bs = PlutusTx.toBuiltin

generateSubTree :: BuiltinByteString -> ProofTree -> Integer -> ProofTree
generateSubTree prefix node depth = do

  go depth
  where
    go :: Integer -> ProofTree
    go n = do

      let  e = even n
           i = 2^(depth PlutusTx.Prelude.- n)
           l = ProofLeaf emptyHash
           (left, right) = if e then (go (n PlutusTx.Prelude.- 1), l) else (l, go (n PlutusTx.Prelude.- 1))
           idx = if e then i else i+1


           va = prefix <> intToBs n
           vb = prefix <> intToBs (n + 1)

      case n of
        0 -> ProofNode (Val idx va vb) node l
        _ -> ProofNode (Val idx va vb) left right


pl = ProofLeaf emptyHash

tt = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1025 "ib5" "ib6") pl (ProofNode (Val 2048 "ib4" "ib5") (ProofNode (Val 4097 "ib3" "ib4") pl (ProofNode (Val 8192 "ib2" "ib3") (ProofNode (Val 16385 "ib1" "ib2") pl (ProofNode (Val 32768 "ib0" "ib1") (ProofNode (Val 65536 "i" "it") pl pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)
tt' = ProofNode (Val 2 "ib15" "ib16") pl (ProofNode (Val 2 "ib14" "ib15") (ProofNode (Val 5 "ib13" "ib14") pl (ProofNode (Val 8 "ib12" "ib13") (ProofNode (Val 17 "ib11" "ib12") pl (ProofNode (Val 32 "ib10" "ib11") (ProofNode (Val 65 "ib9" "ib10") pl (ProofNode (Val 128 "ib8" "ib9") (ProofNode (Val 257 "ib7" "ib8") pl (ProofNode (Val 512 "ib6" "ib7") (ProofNode (Val 1025 "ib5" "ib6") pl (ProofNode (Val 2048 "ib4" "ib5") (ProofNode (Val 4097 "ib3" "ib4") pl (ProofNode (Val 8192 "ib2" "ib3") (ProofNode (Val 16385 "ib1" "ib2") pl (ProofNode (Val 32768 "ib0" "ib1") (ProofNode (Val 65536 "i" "ilap") (ProofNode (Val 131072 "ilap" "it") pl pl) pl) pl)) pl)) pl)) pl)) pl)) pl)) pl)) pl)



main :: IO ()
main = do

  let n = 20
      -- l = ProofLeaf emptyHash
      idx = 2^(n+1)
      un = Val idx "i" "it"
      un' = Val idx "i" "ilap"
      leaf = ProofNode (Val (2*idx) "ilap" "it") pl pl
      updateNode = ProofNode un  pl pl
      appendedNode = ProofNode un' leaf pl
      proof = generateSubTree "ib" updateNode n
      proof' = generateSubTree "ib" appendedNode n
      root = rootHash' proof
      root' = rootHash' proof'

  -- let subtree = generatePath 15 (ProofNode (Val 123 "a" "aaa") (ProofLeaf emptyHash) (ProofLeaf emptyHash))
  print root
  print root'
  Haskell.putStrLn Prelude.$ "Generated subtree: " ++ Haskell.show proof

  Haskell.putStrLn Prelude.$ "Generated subtree: " ++ Haskell.show proof'
  print ( checkAppend "ilap" 131072 root root' un un proof)

{-
  let oth = rootHash' labelTree
      nth = rootHash' labelTree2
      initVal = Val 0 "h" "j"
      initTree = ProofNode initVal eh eh
      appendedTree = ProofNode
                            (Val 0 "h" "ilap")
                            (ProofNode (Val 1 "ilap" "j") eh eh)
                            eh
      ih = rootHash' initTree
      ah = rootHash' appendedTree
  print ih
  print ah
  print Haskell.$ checkAppend "ilap" 1 ih ah initVal initVal initTree
  
  -- print $ checkAppend "ubul" 19 val9 val9 Original oth labelTree
  -- print $ checkAppend "ubul" 19 val9 val9 Updated nth labelTree

  -- print Haskell.$ checkAppend "ilap" 2 oth nth val9 val9 labelTree
-}