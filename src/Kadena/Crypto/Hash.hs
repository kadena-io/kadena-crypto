-- | 

module Kadena.Crypto.Hash (
  -- * Representing a hash
  Hash,

  -- * Producing hashes

  -- ** Specific hashing functions
  hashSha2_256,
  hashSha2_512,
  hashSha3_256,
  hashSha3_512,
  hashKeccak256,
  hashKeccak512,
  hashBlake2s256,
  hashBlake2b512,

  -- ** Generic hashing
  HashAlgo,
  hash,

  -- ** Unsafe hash construction
  unsafeWrap,

  -- * Consuming hashes
  toHexString
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

import Kadena.Crypto.Internal (Hash(Hash), unHash)
import Kadena.Crypto.Hash.Blake2 (Blake2s256, hashBlake2s256, Blake2b512, hashBlake2b512)
import Kadena.Crypto.Hash.Sha2 (Sha2_256, hashSha2_256, Sha2_512, hashSha2_512)
import Kadena.Crypto.Hash.Sha3 (Sha3_256, hashSha3_256, Sha3_512, hashSha3_512)
import Kadena.Crypto.Hash.Keccak (Keccak256, hashKeccak256, Keccak512, hashKeccak512)

toHexString :: Hash a -> String
toHexString = show

-- | If you have a bytestring that you are sure is produced
-- by some hashing algorithm "a", you can unsafely produce
-- an `Hash a`.
unsafeWrap :: ShortByteString -> Hash a
unsafeWrap bs = Hash bs

-- | Any type that can hash a bytestring into a `Hash`.
class HashAlgo algo where
  hash :: ByteString -> Hash algo

-- Blake2
instance HashAlgo Blake2s256 where
  hash = hashBlake2s256

instance HashAlgo Blake2b512 where
  hash = hashBlake2b512

-- Sha2
instance HashAlgo Sha2_256 where
  hash = hashSha2_256

instance HashAlgo Sha2_512 where
  hash = hashSha2_512

-- Sha3
instance HashAlgo Sha3_256 where
  hash = hashSha3_256

instance HashAlgo Sha3_512 where
  hash = hashSha3_512

-- Keccak
instance HashAlgo Keccak256 where
  hash = hashKeccak256

instance HashAlgo Keccak512 where
  hash = hashKeccak512
