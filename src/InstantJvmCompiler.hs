module Main where

import Common (parseAndGenFile)
import JvmGenerator.Generator (generateJvm)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeDirectory)
import System.Process (runCommand, waitForProcess)

compileJvm :: FilePath -> IO ()
compileJvm fileName = do
  let newFileName = replaceExtension fileName ".j"
  parseAndGenFile fileName newFileName (generateJvm fileName)
  let fileParent = takeDirectory fileName
  runHandle <- runCommand $ "java -jar lib/jasmin.jar -d " ++ fileParent ++ " " ++ newFileName
  waitForProcess runHandle
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> compileJvm fileName
    _ -> putStrLn "Usage: ./insc_jvm <ins file path>"
