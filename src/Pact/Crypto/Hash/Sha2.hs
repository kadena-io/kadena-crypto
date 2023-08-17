{-# LANGUAGE TypeApplications #-}

-- | 

module Pact.Crypto.Hash.Sha2
  ( Sha2_256
  , hashSha2_256
  , Sha2_512
  , hashSha2_512
  ) where

import qualified Data.Hash.SHA2 as H
import Data.ByteString (ByteString)
import Pact.Crypto.Hash.Internal (PactHash(..))

-- | A phantom type for marking sha2-256 hashes.
data Sha2_256

-- | Hash a ByteString using the sha2-256 algorithm.
hashSha2_256 :: ByteString -> PactHash Sha2_256
hashSha2_256 v = PactHash sbs
  where H.Sha2_256 sbs = H.hashByteString @H.Sha2_256 v

-- | A phantom type for marking sha2-512 hashes.
data Sha2_512

-- | Hash a ByteString using the sha2-512 algorithm.
hashSha2_512 :: ByteString -> PactHash Sha2_512
hashSha2_512 v = PactHash sbs
  where H.Sha2_512 sbs = H.hashByteString @H.Sha2_512 v
