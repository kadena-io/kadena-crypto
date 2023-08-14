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

-- -- | A value-level enumeration of the available hashing algorithms.
-- data HashAlgorithm
--  = Sha2_256
--  | Sha2_512
--  | Sha3_256
--  | Sha3_512
--  | Keccak256
--  | Keccak512
--  | Blake2s256
--  | Blake2b256
