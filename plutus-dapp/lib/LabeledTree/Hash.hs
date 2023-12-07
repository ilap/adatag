{-# LANGUAGE TemplateHaskell #-}

module LabeledTree.Hash where

import qualified Data.ByteString.Base16 as Haskell.Base16
import qualified Data.Text as Haskell.Text
import qualified Data.Text.Encoding as Haskell.Text.Encoding
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

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

PlutusTx.unstableMakeIsData ''Hash
