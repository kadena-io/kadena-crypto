-- | 

module Kadena.Crypto.Signature where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Kadena.Crypto.Internal (PublicKey(..), Signature(..))
import Kadena.Crypto.Signature.ECDSA (ECDSA, verifySignatureECDSA)
import Kadena.Crypto.Signature.Ed25519 (Ed25519, verifySignatureEd25519)

class SignatureAlgo algo where
  verifySignature :: PublicKey algo -> ByteString -> Signature algo -> Bool

instance SignatureAlgo ECDSA where
  verifySignature = verifySignatureECDSA

instance SignatureAlgo Ed25519 where
  verifySignature = verifySignatureEd25519

unsafePublicKeyFromHex :: Text -> Either String (PublicKey algo)
unsafePublicKeyFromHex hex = case Base16.decode (encodeUtf8 hex) of
  Right bytes -> Right $ PublicKey (SBS.toShort bytes)
  Left e -> Left e

unsafeSignatureFromHex :: Text -> Either String (Signature algo)
unsafeSignatureFromHex hex = case Base16.decode (encodeUtf8 hex) of
  Right bytes -> Right $ Signature (SBS.toShort bytes)
  Left e -> Left e
