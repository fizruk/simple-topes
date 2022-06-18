module Main where

import qualified RSTT.Interpret as RSTT

main :: IO ()
main = do
  input <- getContents
  RSTT.interpretIO input
