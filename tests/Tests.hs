{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Pact.Crypto.Hash (unPactHash, toHexString)
import Pact.Crypto.Hash.Sha2 (hashSha2_256, hashSha2_512, Sha2_256, Sha2_512)
import Pact.Crypto.Hash.Sha3 (hashSha3_256, hashSha3_512, Sha3_256, Sha3_512)

import Pact.Crypto.Hash.Blake2 (hashBlake2s256, hashBlake2b512, Blake2s256, Blake2b512)
import Pact.Crypto.Hash.Keccak (hashKeccak256, hashKeccak512, Keccak256, Keccak512)

main :: IO ()
main =  defaultMain tests

tests :: TestTree
tests = testGroup "Hash algorithms match openssl"
  [ -- sha2
    testProperty "sha2_256-property" sha2_256Prop
  , testCase "sha2_256-example" sha2_256Example
  -- sha2-512
  , testProperty "sha2_512-property" sha2_512Prop
  , testCase "sha2_512-example" sha2_512Example
  -- sha3
  , testProperty "sha3_256-property" sha3_256Prop
  , testCase "sha3_256-example" sha3_256Example
  -- sha3-512
  , testProperty "sha3_512-property" sha3_512Prop
  , testCase "sha3_512-example" sha3_512Example
  -- blake2b
  , testProperty "blake2b512-property" blake2b512Prop
  , testCase "blake2b512-example" blake2b512Example
  -- blake2s
  , testProperty "blake2s256-property" blake2s256Prop
  , testCase "blake2s256-example" blake2s256Example
  -- keccak256
  , testCase "keccak256-example" keccak256Example
  -- keccak512
  , testCase "keccak512-example" keccak512Example
  -- class
  , testProperty "class-based hash" classHashProp
  ]

sha2_256Example :: IO ()
sha2_256Example =  assertEqual "Hash should match" (show pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha2_256 bytes
    expected = "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"

sha2_512Example :: IO ()
sha2_512Example =  assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha2_512 bytes
    expected = "e7c22b994c59d9cf2b48e549b1e24666636045930d3da7c1acb299d1c3b7f931f94aae41edda2c2b207a36e10f8bcb8d45223e54878f5b316e7ce3b6bc019629"

sha3_256Example :: IO ()
sha3_256Example =  assertEqual "Hash should match" (show pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha3_256 bytes
    expected = "b314e28493eae9dab57ac4f0c6d887bddbbeb810e900d818395ace558e96516d"

sha3_512Example :: IO ()
sha3_512Example =  assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashSha3_512 bytes
    expected = "ac766ba623301e0ad63c48cb2fc469d10145f65c9f1f28fe761c78c386ed295a1fda1b05e280354e620757d8a83e05a45f66438dd734278668c1c27ac6f27150"

blake2b512Example :: IO ()
blake2b512Example = assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashBlake2b512 bytes
    expected = "f60ce482e5cc1229f39d71313171a8d9f4ca3a87d066bf4b205effb528192a75f14f3271e2c1a90e1de53f275b4d4793eef2f5e31ea90d2ce29d2e481c36435f"

blake2s256Example :: IO ()
blake2s256Example = assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello\n"
    pactHash = hashBlake2s256 bytes
    expected = "3969b3926654065966b6f8d9a65789b0f76d56e1e2ab67dd94faa770959187ca"

keccak256Example :: IO ()
keccak256Example = assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello"
    pactHash = hashKeccak256 bytes
    expected = "1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8"

keccak512Example :: IO ()
keccak512Example = assertEqual "Hash should match" (toHexString pactHash) expected
  where
    bytes = BSC.pack "hello"
    pactHash = hashKeccak512 bytes
    expected = "52fa80662e64c128f8389c9ea6c73d4c02368004bf4463491900d11aaadca39d47de1b01361f207c512cfa79f0f92c3395c67ff7928e3f5ce3e3c852b392f976"

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

sha2_256Prop :: [Word8] -> Property
sha2_256Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha2_256 bytes
  opensslOutput <- liftIO $ callOpenssl "sha256" bytes
  assert (toHexString pactHash == opensslOutput)

sha2_512Prop :: [Word8] -> Property
sha2_512Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha2_512 bytes
  opensslOutput <- liftIO $ callOpenssl "sha512" bytes
  assert (toHexString pactHash == opensslOutput)

sha3_256Prop :: [Word8] -> Property
sha3_256Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha3_256 bytes
  opensslOutput <- liftIO $ callOpenssl "sha3-256" bytes
  assert (toHexString pactHash == opensslOutput)

sha3_512Prop :: [Word8] -> Property
sha3_512Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashSha3_512 bytes
  opensslOutput <- liftIO $ callOpenssl "sha3-512" bytes
  assert (toHexString pactHash == opensslOutput)

blake2s256Prop :: [Word8] -> Property
blake2s256Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashBlake2s256 bytes
  opensslOutput <- liftIO $ callOpenssl "blake2s256" bytes
  assert (toHexString pactHash == opensslOutput)

blake2b512Prop :: [Word8] -> Property
blake2b512Prop bs = monadicIO $ do
  let
    bytes = BS.pack bs
    pactHash = hashBlake2b512 bytes
  opensslOutput <- liftIO $ callOpenssl "blake2b512" bytes
  assert (toHexString pactHash == opensslOutput)

classHashProp :: [Word8] -> Property
classHashProp bs = monadicIO $ do
  let bytes = BS.pack bs
  assert (hashSha2_256 bytes == pactHash @Sha2_256 bytes)
  assert (hashSha2_512 bytes == pactHash @Sha2_512 bytes)

  assert (hashSha3_256 bytes == pactHash @Sha3_256 bytes)
  assert (hashSha3_512 bytes == pactHash @Sha3_512 bytes)

  assert (hashBlake2s256 bytes == pactHash @Blake2s256 bytes)
  assert (hashBlake2b512 bytes == pactHash @Blake2b512 bytes)

  assert (hashKeccak256 bytes == pactHash @Keccak256 bytes)
  assert (hashKeccak512 bytes == pactHash @Keccak512 bytes)
