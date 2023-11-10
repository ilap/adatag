{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Plutus.LabeledTree where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, take)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.UTF8 (fromString, toString)
import Numeric
import System.IO
import Prelude hiding (putStr, take)

log2 :: Integer -> Integer
log2 x = if odd x then 0 else (floor . logBase (2 :: Double) . fromIntegral) x

data Node
  = Leaf !ByteString
  | Node !Integer !(ByteString, ByteString) !Node !Node

-- Leaf
leaf :: Node
leaf = Leaf (fromString "")

index :: Node -> Integer
index (Leaf _) = -1
index (Node i _ _ _) = i

xa :: Node -> ByteString
xa (Leaf s) = s
xa (Node _ (a, _) _ _) = a

xb :: Node -> ByteString
xb (Leaf s) = s
xb (Node _ (_, b) _ _) = b

-- TODO: Incomplete pattern
left :: Node -> Node
left (Node _ _ l _) = l

-- TODO: Incomplete pattern
right :: Node -> Node
right (Node _ _ _ r) = r

-- Get the value H(index || xa || xb) of a node.
val :: Node -> ByteString
val (Leaf s) = s
val (Node i (a, b) _ _) = do
  let v = fromString (show i) <> a <> b
  hash v

-- Get the proof a node.
{-# INLINEABLE proof #-}
proof :: Node -> ByteString
proof (Leaf a) = a
proof (Node i (a, b) l r) = do
  let v = hash (fromString (show i) <> a <> b)
  hash (v <> proof l <> proof r)

-- Print the proof of a node to the console.
printProof :: Node -> IO ()
printProof (Leaf s) = putStrLn $ toString s
printProof n =
  print $ take 8 (B16.encode (proof n))

hex :: ByteString -> String
hex = toString . B16.encode

printTree :: Node -> String -> IO ()
printTree (Leaf s) prefix = putStr $ prefix ++ toString s
printTree node prefix = do
  -- let level = log2 (index node)
  putStrLn $ prefix ++ " |  " ++ show (index node) ++ " (" ++ toString (xa node) ++ ", " ++ toString (xb node) ++ "), val: \"" ++ hex (take 4 (val node)) ++ "\", proof: \"" ++ hex (take 4 (proof node)) ++ "\", pL: \"" ++ hex (take 4 (proof (left node))) ++ "\", pR: \"" ++ hex (take 4 (proof (right node))) ++ "\""
  printTree (left node) (prefix ++ "   ")
  printTree (right node) (prefix ++ "   ")

{-# INLINEABLE checkUpdate #-}
checkUpdate :: ByteString -> (ByteString, ByteString) -> (ByteString, ByteString) -> Node -> Bool -> Node
checkUpdate _ _ _ (Leaf s) _ = Leaf s
checkUpdate x nu na node isLeft = do
  let isSameNode = nu == na
  let (xa', xb') = nu
  let (xa'', xb'') = na
  if xa' == xa node && x == xb node
    then -- Found the Nu

      if isSameNode -- it's the Na too
        then
          if isLeft
            then Node (index node) (xa', xb') leaf (right node)
            else Node (index node) (xa', xb') (left node) leaf
        else Node (index node) (xa', xb') (left node) (right node)
    else
      if xa'' == xa node && xb'' == xb node
        then -- Found the Na

          if isLeft
            then Node (index node) (xa'', xb'') (left node) leaf
            else Node (index node) (xa'', xb'') leaf (right node)
        else Node (index node) (xa node, xb node) (checkUpdate x (xa', xb') (xa'', xb'') (left node) isLeft) (checkUpdate x (xa', xb') (xa'', xb'') (right node) isLeft)

-- where
--  newLeft =  if index == index node then Leaf "" else left node
--  newRight = if index == index node then Leaf "" else right node
