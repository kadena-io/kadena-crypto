-- | 

module Kadena.Crypto.Signature.ECDSA
  ( ECDSA
  , verifySignatureECDSA
  ) where

import Data.ByteString (ByteString)
import Kadena.Crypto.Internal (PublicKey(..), Signature(..))

data ECDSA

verifySignatureECDSA :: PublicKey ECDSA -> ByteString -> Signature ECDSA -> Bool
verifySignatureECDSA (PublicKey pk) bs (Signature sig) = undefined
