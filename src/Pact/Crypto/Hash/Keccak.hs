{-# LANGUAGE TypeApplications #-}

-- | 

module Pact.Crypto.Hash.Keccak
  ( Keccak256
  , hashKeccak256
  , Keccak512
  , hashKeccak512
  ) where

import qualified Data.Hash.Keccak as H
import Data.ByteString (ByteString)
import Pact.Crypto.Types (PactHash(..))

-- | A phantom type for marking keccack-256 hashes.
data Keccak256

-- | Hash a ByteString using the keccack-256 algorithm.
hashKeccak256 :: ByteString -> PactHash Keccak256
hashKeccak256 v = PactHash sbs
  where H.Keccak256 sbs = H.hashByteString @H.Keccak256 v

-- | A phantom type for marking keccack-512 hashes.
data Keccak512

-- | Hash a ByteString using the keccack-512 algorithm.
hashKeccak512 :: ByteString -> PactHash Keccak512
hashKeccak512 v = PactHash sbs
  where H.Keccak512 sbs = H.hashByteString @H.Keccak512 v
