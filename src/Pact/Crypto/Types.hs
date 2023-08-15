-- | 

module Pact.Crypto.Types where


import Data.ByteString.Short (ShortByteString, unpack)
import Text.Printf (printf)

-- | A `PactHash` is a `ShortByteString` representing the result
-- of running the hash algorithm specified by `PactHash`'s type
-- parameter.
newtype PactHash algo
  = PactHash { unPactHash :: ShortByteString }

instance Show (PactHash a) where
  show (PactHash b) = concatMap (printf "%0.2x") (unpack b)

toHexString :: PactHash a -> String
toHexString = show
