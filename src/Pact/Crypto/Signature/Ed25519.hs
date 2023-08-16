-- | 

module Pact.Crypto.Signature.Ed25519
  ( Ed25519
  , verifySignatureEd25519
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Pact.Crypto.Types (PactPublicKey(..), PactSignature(..))
import qualified Crypto.PubKey.Ed25519 as C
import qualified Crypto.Error as C

data Ed25519

verifySignatureEd25519 :: PactPublicKey Ed25519 -> ByteString -> PactSignature Ed25519 -> Bool
verifySignatureEd25519 (PactPublicKey pk) bs (PactSignature sig) = case cpk of
  C.CryptoPassed (ckey, csig) -> C.verify ckey bs csig
  C.CryptoFailed _ -> False
  where
    cpk = liftA2 (,) (C.publicKey (fromShort pk)) (C.signature (fromShort sig))
