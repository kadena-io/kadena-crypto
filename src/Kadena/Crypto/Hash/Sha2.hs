{-# LANGUAGE TypeApplications #-}

-- | 

module Kadena.Crypto.Hash.Sha2
  ( Sha2_256
  , hashSha2_256
  , Sha2_512
  , hashSha2_512
  ) where

import qualified Data.Hash.SHA2 as H
import Data.ByteString (ByteString)
import Kadena.Crypto.Internal (Hash(..))

-- | A phantom type for marking sha2-256 hashes.
data Sha2_256

-- | Hash a ByteString using the sha2-256 algorithm.
hashSha2_256 :: ByteString -> Hash Sha2_256
hashSha2_256 v = Hash sbs
  where H.Sha2_256 sbs = H.hashByteString @H.Sha2_256 v

-- | A phantom type for marking sha2-512 hashes.
data Sha2_512

-- | Hash a ByteString using the sha2-512 algorithm.
hashSha2_512 :: ByteString -> Hash Sha2_512
hashSha2_512 v = Hash sbs
  where H.Sha2_512 sbs = H.hashByteString @H.Sha2_512 v
