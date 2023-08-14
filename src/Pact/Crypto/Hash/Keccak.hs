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

data Keccak256
hashKeccak256 :: ByteString -> PactHash Keccak256
hashKeccak256 v = PactHash sbs
  where H.Keccak256 sbs = H.hashByteString @H.Keccak256 v

data Keccak512
hashKeccak512 :: ByteString -> PactHash Keccak512
hashKeccak512 v = PactHash sbs
  where H.Keccak512 sbs = H.hashByteString @H.Keccak512 v
