{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

module IntegriTree.Hash where

import Data.ByteString.Base16 qualified as Haskell.Base16
import Data.Text qualified as Haskell.Text
import Data.Text.Encoding qualified as Haskell.Text.Encoding
import PlutusTx qualified
import PlutusTx.Prelude hiding (toList)
import Prelude qualified as Haskell

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

{-# INLINABLE hash #-}
hash :: BuiltinByteString -> Hash
hash = Hash .  sha2_256

{-# INLINABLE combineHashes #-}
combineHashes :: Hash -> Hash -> Hash
combineHashes (Hash h) (Hash h') = hash (appendByteString h h')

-- * combineThreeHashes

-- {-# INLINABLE combineThreeHashes #-}
combineThreeHashes :: Hash -> Hash -> Hash -> Hash
combineThreeHashes (Hash h) (Hash h') (Hash h'') =
  hash (appendByteString h (appendByteString h' h''))

PlutusTx.unstableMakeIsData ''Hash
