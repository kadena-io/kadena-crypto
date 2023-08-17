module Kadena.Crypto.Internal where

import Data.ByteString.Short (ShortByteString, unpack)
import Text.Printf (printf)

-- | A `PactHash` is a `ShortByteString` representing the result
-- of running the hash algorithm specified by `PactHash`'s type
-- parameter.
--
-- The constructor is hidden, to encourage the use of the
-- concrete hashing functions, such as `hashSha3_256` and `hashBlake2b_256`.
newtype Hash algo
  = Hash { unHash :: ShortByteString }
  deriving Eq

-- | We show hashes by hex encoding them.
instance Show (Hash a) where
  show (Hash b) = concatMap (printf "%0.2x") (unpack b)

newtype PublicKey algo
  = PublicKey { unPublicKey :: ShortByteString }
  deriving Eq

instance Show (PublicKey algo) where
  show (PublicKey b) = concatMap (printf "%0.2x") (unpack b)

newtype Signature algo
  = Signature { unSignature :: ShortByteString }
  deriving Eq

instance Show (Signature algo) where
  show (Signature b) = concatMap (printf "%0.2x") (unpack b)
