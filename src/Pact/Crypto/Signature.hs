-- | 

module Pact.Crypto.Signature where

import Data.ByteString (ByteString)
import Pact.Crypto.Types (PactPublicKey, PactSignature)
import Pact.Crypto.Signature.ECDSA (ECDSA, verifySignatureECDSA)
import Pact.Crypto.Signature.Ed25519 (Ed25519, verifySignatureEd25519)

class PactSignatureAlgo algo where
  pactVerifySignature :: PactPublicKey algo -> ByteString -> PactSignature algo -> Bool

instance PactSignatureAlgo ECDSA where
  pactVerifySignature = verifySignatureECDSA

instance PactSignatureAlgo Ed25519 where
  pactVerifySignature = verifySignatureEd25519
