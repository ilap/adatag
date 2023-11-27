-- {-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Plutus.OldLabeledTree where

{- FIXME:
import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString, take)
import qualified Data.ByteString.Base16 as B16 -- used by data-default
import Data.ByteString.UTF8 (fromString, toString)
import Numeric
import System.IO
import Prelude hiding (putStr, take)
import PlutusLedgerApi.V1
import qualified Data.ByteString.Base16 as Haskell.Base16
import qualified Data.Text as Haskell.Text
import qualified Data.Text.Encoding as Haskell.Text.Encoding -- text
log2 :: Integer -> Integer
log2 x = if odd x then 0 else (floor . logBase (2 :: Double) . fromIntegral) x


-- ####### DATUM

-- | A type for representing hash digests.
newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq)

unstableMakeIsData ''Hash

instance Eq Hash where
  (==) :: Hash -> Hash -> Bool
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) =
    Haskell.Text.unpack
      . Haskell.Text.Encoding.decodeUtf8
      . Haskell.Base16.encode
      . fromBuiltin
      . takeByteString 4
      $ bs

{-# INLINEABLE hash #-}
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256

{-# INLINEABLE combineHash #-}
combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')

c2bs :: Integer -> BuiltinByteString
c2bs i = stringToBuiltinByteString $ Haskell.show i

  let c = indexByteString adatag 0
      l = c - 1
      r = c + 1
      v = c2bs l <> c2bs r
------------------------------------ LABELED TREE MOCKS -------------------------------------------
-- FIXME: These are vill be in the LabeledTree library

-- | Node data structure represents a node of the Labeled Complete Binary Tree.
{- newtype Val = Val (BuiltinByteString, BuiltinByteString)
  -- data Val = Val (BuiltinByteString, BuiltinByteString)
  deriving (Show)

PlutusTx.unstableMakeIsData ''Val

toVal :: BuiltinByteString -> BuiltinByteString -> Val
toVal bs1 bs2 = Val (bs1, bs2)

fromVal :: Val -> (BuiltinByteString, BuiltinByteString)
fromVal (Val (bs1, bs2)) = (bs1, bs2)

data Tag = Tag BuiltinInteger Val
  deriving (Show)

PlutusTx.unstableMakeIsData ''Tag

data Node
  = Leaf BuiltinByteString
  | Node BuiltinInteger (BuiltinByteString, BuiltinByteString) Node Node
  deriving (Show)

PlutusTx.unstableMakeIsData ''Node

-}

newtype Hash = Hash BuiltinByteString

data Node
  = Leaf !ByteString
  | Node !(ByteString, ByteString) !Node !Node
  | HashNode !Hash !Node !Node


-- Leaf
leaf :: Node
leaf = Leaf (fromString "")

xa :: Node -> ByteString
xa (Leaf s) = s
xa (Node (a, _) _ _) = a

xb :: Node -> ByteString
xb (Leaf s) = s
xb (Node (_, b) _ _) = b

-- TODO: Incomplete pattern
left :: Node -> Node
left (Node _ l _) = l

-- TODO: Incomplete pattern
right :: Node -> Node
right (Node _ _ r) = r

-- Get the value H(index || xa || xb) of a node.
val :: Node -> ByteString
val (Leaf s) = s
val (Node (a, b) _ _) = do
  let v = a <> b
  hash v

-- Get the proof a node.
{-# INLINEABLE proof #-}
proof :: Node -> ByteString
proof (Leaf a) = a
proof (Node (a, b) l r) = do
  let v = hash a <> b
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
  putStrLn $ prefix ++ " |  " ++ " (" ++ toString (xa node) ++ ", " ++ toString (xb node) ++ "), val: \"" ++ hex (take 4 (val node)) ++ "\", proof: \"" ++ hex (take 4 (proof node)) ++ "\", pL: \"" ++ hex (take 4 (proof (left node))) ++ "\", pR: \"" ++ hex (take 4 (proof (right node))) ++ "\""
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
            then Node (xa', xb') leaf (right node)
            else Node (xa', xb') (left node) leaf
        else Node (xa', xb') (left node) (right node)
    else
      if xa'' == xa node && xb'' == xb node
        then -- Found the Na

          if isLeft
            then Node (xa'', xb'') (left node) leaf
            else Node (xa'', xb'') leaf (right node)
        else Node (xa node, xb node) (checkUpdate x (xa', xb') (xa'', xb'') (left node) isLeft) (checkUpdate x (xa', xb') (xa'', xb'') (right node) isLeft)

-- where
--  newLeft =  if index == index node then Leaf "" else left node
--  newRight = if index == index node then Leaf "" else right node
-}
