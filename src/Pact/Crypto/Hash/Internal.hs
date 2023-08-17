-- |

module Pact.Crypto.Hash.Internal where

import Data.ByteString.Short (ShortByteString, unpack)
import Text.Printf (printf)

-- | A `PactHash` is a `ShortByteString` representing the result
-- of running the hash algorithm specified by `PactHash`'s type
-- parameter.
--
-- The constructor is hidden, to encourage the use of the
-- concrete hashing functions, such as `hashSha3_256` and `hashBlake2b_256`.
newtype PactHash algo
  = PactHash { unPactHash :: ShortByteString }
  deriving Eq

-- | We show hashes by hex encoding them.
instance Show (PactHash a) where
  show (PactHash b) = concatMap (printf "%0.2x") (unpack b)
