-- | 

module Kadena.Crypto.Signature.Ed25519
  ( Ed25519
  , verifySignatureEd25519
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Kadena.Crypto.Internal (PublicKey(..), Signature(..))

import qualified Crypto.PubKey.Ed25519 as C
import qualified Crypto.Error as C

data Ed25519

verifySignatureEd25519 :: PublicKey Ed25519 -> ByteString -> Signature Ed25519 -> Bool
verifySignatureEd25519 (PublicKey pk) bs (Signature sig) = case cpk of
  C.CryptoPassed (ckey, csig) -> C.verify ckey bs csig
  -- It is safe to simply return `False` in the `C.CryptoFailed` case (which
  -- occurs if parsing fails), because the `PublicKey` and `Signature` constructors
  -- for the arguments are smart constructors that prove the bytestrings
  -- they contain will not fail to parse.
  -- In other words, `C.CryptoFailed` is an unreachable code path.
  C.CryptoFailed _ -> False
  where
    cpk = liftA2 (,) (C.publicKey (fromShort pk)) (C.signature (fromShort sig))
