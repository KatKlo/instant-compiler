module Main where

import Common (parseAndGenFile)
import LlvmGenerator.Generator (generateLlvm)
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Process (runCommand, waitForProcess)

compileJvm :: FilePath -> IO ()
compileJvm fileName = do
  let newFileName = replaceExtension fileName ".ll"
  parseAndGenFile fileName newFileName generateLlvm
  let bcFileName = replaceExtension fileName ".bc"
  runHandle <- runCommand $ "llvm-as -o " ++ bcFileName ++ " " ++ newFileName
  waitForProcess runHandle
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> compileJvm fileName
    _ -> putStrLn "Usage: ./insc_llvm <ins file path>"
