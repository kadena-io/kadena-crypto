-- | 

module Pact.Crypto.Types where


import Data.ByteString.Short (ShortByteString, unpack)
import Text.Printf (printf)

newtype PactHash algo
  = PactHash { unPactHash :: ShortByteString }

instance Show (PactHash a) where
  show (PactHash b) = concatMap (printf "%0.2x") (unpack b)
