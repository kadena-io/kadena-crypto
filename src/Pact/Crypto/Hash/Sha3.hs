{-# LANGUAGE TypeApplications #-}

-- | 

module Pact.Crypto.Hash.Sha3
  ( Sha3_256
  , hashSha3_256
  , Sha3_512
  , hashSha3_512
  ) where

import qualified Data.Hash.SHA3 as H
import Data.ByteString (ByteString)
import Pact.Crypto.Hash.Internal (PactHash(..))

-- | A phantom type for marking sha3-256 hashes.
data Sha3_256

-- | Hash a ByteString using the sha3-256 algorithm.
hashSha3_256 :: ByteString -> PactHash Sha3_256
hashSha3_256 v = PactHash sbs
  where H.Sha3_256 sbs = H.hashByteString @H.Sha3_256 v

-- | A phantom type for marking sha3-512 hashes.
data Sha3_512

-- | Hash a ByteString using the sha3-512 algorithm.
hashSha3_512 :: ByteString -> PactHash Sha3_512
hashSha3_512 v = PactHash sbs
  where H.Sha3_512 sbs = H.hashByteString @H.Sha3_512 v
