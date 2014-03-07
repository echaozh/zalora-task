module Main where

import qualified Data.ByteString.Char8 as BS

import System.Environment (getArgs)

import Web.ZaloraTask

main ∷ IO ()
main = do
  port : dir : connStr : _ ← getArgs
  zalora (read port) dir (BS.pack connStr) 10 100
