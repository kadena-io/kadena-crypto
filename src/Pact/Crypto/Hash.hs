-- | 

module Pact.Crypto.Hash (
  -- * Representing a hash
  PactHash,

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
  PactHashAlgo,
  pactHash,

  -- ** Unsafe hash construction
  unsafeWrap,

  -- * Consuming hashes
  toHexString,
  unPactHash

) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

import Pact.Crypto.Hash.Internal (PactHash(PactHash), unPactHash)
import Pact.Crypto.Hash.Blake2 (Blake2s256, hashBlake2s256, Blake2b512, hashBlake2b512)
import Pact.Crypto.Hash.Sha2 (Sha2_256, hashSha2_256, Sha2_512, hashSha2_512)
import Pact.Crypto.Hash.Sha3 (Sha3_256, hashSha3_256, Sha3_512, hashSha3_512)
import Pact.Crypto.Hash.Keccak (Keccak256, hashKeccak256, Keccak512, hashKeccak512)

toHexString :: PactHash a -> String
toHexString = show

-- | If you have a bytestring that you are sure is produced
-- by some hashing algorithm "a", you can unsafely produce
-- an `PactHash a`.
unsafeWrap :: ShortByteString -> PactHash a
unsafeWrap bs = PactHash bs

-- | Any type that can hash a bytestring into a `PactHash`.
class PactHashAlgo algo where
  pactHash :: ByteString -> PactHash algo

-- Blake2
instance PactHashAlgo Blake2s256 where
  pactHash = hashBlake2s256

instance PactHashAlgo Blake2b512 where
  pactHash = hashBlake2b512

-- Sha2
instance PactHashAlgo Sha2_256 where
  pactHash = hashSha2_256

instance PactHashAlgo Sha2_512 where
  pactHash = hashSha2_512

-- Sha3
instance PactHashAlgo Sha3_256 where
  pactHash = hashSha3_256

instance PactHashAlgo Sha3_512 where
  pactHash = hashSha3_512

-- Keccak
instance PactHashAlgo Keccak256 where
  pactHash = hashKeccak256

instance PactHashAlgo Keccak512 where
  pactHash = hashKeccak512
