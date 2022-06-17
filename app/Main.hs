module Main where

import qualified RSTT.Interpret     as RSTT
import qualified RSTT.Syntax.Layout as RSTT
import qualified RSTT.Syntax.Par    as RSTT

main :: IO ()
main = do
  input <- getContents
  let tokens = RSTT.resolveLayout True (RSTT.myLexer input)
  case RSTT.pProgram tokens of
    Left err -> do
      putStrLn "ERROR: Syntax error:"
      putStrLn err
    Right program -> RSTT.interpret program
