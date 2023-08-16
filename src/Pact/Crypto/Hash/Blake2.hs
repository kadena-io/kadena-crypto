{-# LANGUAGE TypeApplications #-}
-- | 

module Pact.Crypto.Hash.Blake2
  ( Blake2s256
  , hashBlake2s256

  , Blake2b512
  , hashBlake2b512
  ) where

import qualified Data.Hash.Blake2 as H
import Data.ByteString (ByteString)
import Pact.Crypto.Hash.Internal (PactHash(..))

-- | A phantom type for marking blake2s-256 hashes.
data Blake2s256

-- | Hash a ByteString using the blake2s-256 algorithm.
hashBlake2s256 :: ByteString -> PactHash Blake2s256
hashBlake2s256 v = PactHash sbs
  where H.Blake2s256 sbs = H.hashByteString @H.Blake2s256 v

-- | A phantom type for marking blake2b-512 hashes.
data Blake2b512

-- | Hash a ByteString using the blake2b-512 algorithm.
hashBlake2b512 :: ByteString -> PactHash Blake2b512
hashBlake2b512 v = PactHash sbs
  where H.Blake2b512 sbs = H.hashByteString @H.Blake2b512 v
