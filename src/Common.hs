module Common where

import Grammar.AbsInstant (Program)
import Grammar.ParInstant (myLexer, pProgram)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

type GenFun = Program -> IO [String]

parseAndGenFile :: FilePath -> FilePath -> GenFun -> IO ()
parseAndGenFile inFileName outFileName genFun =
  readFile inFileName >>= parseFile >>= gen genFun >>= writeFile outFileName

parseFile :: String -> IO Program
parseFile s =
  case pProgram (myLexer s) of
    Left err -> do
      putStrLn "Parse failed..."
      hPrint stderr err
      exitFailure
    Right tree -> do
      putStrLn "Parse Successful!"
      pure tree

gen :: GenFun -> Program -> IO String
gen fun prog = do
  progResult <- fun prog
  putStrLn "Generating successful!"
  return . unlines $ progResult
