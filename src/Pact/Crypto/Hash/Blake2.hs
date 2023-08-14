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
import Pact.Crypto.Types (PactHash(..))

data Blake2s256
hashBlake2s256 :: ByteString -> PactHash Blake2s256
hashBlake2s256 v = PactHash sbs
  where H.Blake2s256 sbs = H.hashByteString @H.Blake2s256 v

data Blake2b512
hashBlake2b512 :: ByteString -> PactHash Blake2b512
hashBlake2b512 v = PactHash sbs
  where H.Blake2b512 sbs = H.hashByteString @H.Blake2b512 v
