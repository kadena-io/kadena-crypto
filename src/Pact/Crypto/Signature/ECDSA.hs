-- | 

module Pact.Crypto.Signature.ECDSA
  ( ECDSA
  , verifySignatureECDSA
  ) where

import Data.ByteString (ByteString)
import Pact.Crypto.Types (PactPublicKey(..), PactSignature(..))

data ECDSA

verifySignatureECDSA :: PactPublicKey ECDSA -> ByteString -> PactSignature ECDSA -> Bool
verifySignatureECDSA (PactPublicKey pk) bs (PactSignature sig) = undefined
