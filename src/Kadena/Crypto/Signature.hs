-- | 

module Kadena.Crypto.Signature where

import Data.ByteString (ByteString)
import Kadena.Crypto.Internal (PublicKey, Signature)
import Kadena.Crypto.Signature.ECDSA (ECDSA, verifySignatureECDSA)
import Kadena.Crypto.Signature.Ed25519 (Ed25519, verifySignatureEd25519)

class SignatureAlgo algo where
  verifySignature :: PublicKey algo -> ByteString -> Signature algo -> Bool

instance SignatureAlgo ECDSA where
  verifySignature = verifySignatureECDSA

instance SignatureAlgo Ed25519 where
  verifySignature = verifySignatureEd25519
