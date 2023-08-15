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
import Test.QuickCheck.Monadic (assert, monadicIO)

import Pact.Crypto.Types (PactHash(PactHash, unPactHash), HashAlgo(..))
import Pact.Crypto.Hash.Sha2 (hashSha2_256)

main :: IO ()
main =  defaultMain tests

tests :: TestTree
tests = testGroup "Hash algorithms match openssl"
  [ testProperty "sha256-property" sha256Prop
  , testCase "sha256-example" sha256Example
  -- , sha512Props
  -- , blake2bProps
  -- , blake2sProps
  -- , keccack256Props
  -- , keccack512Props
  -- , classProps
  ]

sha256Example = IO ()
sha256Example = do
  let
    bytes = BS.pack [0,0,0]
    pactHash = hashSha2_256 bytes
  expected =

callOpenssl :: String -> BS.ByteString -> IO String
callOpenssl hash

sha256Prop (bs :: [Word8]) = monadicIO $ do

  let
    bytes = BS.pack bs
    pactHash = hashSha2_256 bytes

  opensslOutput <- liftIO $ do
    (Just hIn, Just hOut, _, pHandle) <- Process.createProcess
      (Process.proc "openssl" ["dgst", "-sha256", "-hex", "-r"])
      { Process.std_in = Process.CreatePipe
      , Process.std_out = Process.CreatePipe
      }
    BS.hPut hIn bytes
    hClose hIn
    result <- hGetContents hOut
    return result

  -- Openssl suffixes hashes with some metadata we want to discard.
  let opensslHex = takeWhile (/= ' ') opensslOutput
  when (show pactHash /= opensslHex) $ liftIO $ do
    putStrLn ""
    putStrLn $ BSC.unpack bytes
    print bs
    print pactHash
    putStrLn opensslHex

  assert (show pactHash == opensslHex)
