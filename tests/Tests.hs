{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as BSS
import Data.Word (Word8)
import System.IO (hClose, hGetContents)
import qualified System.Process as Process
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.QuickCheck.Monadic (assert, monadicIO)

import Pact.Crypto.Types (PactHash(PactHash, unPactHash), toHexString)
import Pact.Crypto.Hash.Sha2 (hashSha2_256, hashSha2_512)

main :: IO ()
main =  defaultMain tests

tests :: TestTree
tests = testGroup "Hash algorithms match openssl"
  [ testProperty "sha256-property" sha256Prop
  , testCase "sha256-example" sha256Example
  -- sha512
  , testProperty "sha512-property" sha512Prop
  , testCase "sha512-example" sha512Example
  -- , blake2bProps
  -- , blake2sProps
  -- , keccack256Props
  -- , keccack512Props
  -- , classProps
  ]

sha256Example :: IO ()
sha256Example =  assertEqual "Hash should match" (show pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha2_256 bytes
    expected = "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"

sha512Example :: IO ()
sha512Example =  assertEqual "Hash should match" (show pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha2_512 bytes
    expected = "e7c22b994c59d9cf2b48e549b1e24666636045930d3da7c1acb299d1c3b7f931f94aae41edda2c2b207a36e10f8bcb8d45223e54878f5b316e7ce3b6bc019629"


callOpenssl :: String -> BS.ByteString -> IO String
callOpenssl hashAlgo bs = liftIO $ do
    (Just hIn, Just hOut, _, pHandle) <- Process.createProcess
      (Process.proc "openssl" ["dgst", "-" ++ hashAlgo, "-hex", "-r"])
      { Process.std_in = Process.CreatePipe
      , Process.std_out = Process.CreatePipe
      }
    BS.hPut hIn bs
    hClose hIn
    opensslOutput <- hGetContents hOut
    pure (takeWhile (/= ' ') opensslOutput)

sha256Prop :: [Word8] -> Property
sha256Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha2_256 bytes
  opensslOutput <- liftIO $ callOpenssl "sha256" bytes
  assert (toHexString pactHash == opensslOutput)

sha512Prop :: [Word8] -> Property
sha512Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha2_512 bytes
  opensslOutput <- liftIO $ callOpenssl "sha512" bytes
  assert (toHexString pactHash == opensslOutput)
