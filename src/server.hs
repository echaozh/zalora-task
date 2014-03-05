module Main where

import qualified Data.ByteString.Char8 as BS

import System.Environment (getArgs)

import Web.ZaloraTask

main ∷ IO ()
main = do
  port : connStr : _ ← getArgs
  zalora (read port) (BS.pack connStr) 10
