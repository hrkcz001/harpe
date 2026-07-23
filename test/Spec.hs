module Main where

import System.Exit (exitSuccess)
import CoreSpec (runCoreTests)
import CompilerSpec (runCompilerTests)
import ParserSpec (runParserTests)

main :: IO ()
main = do
  putStrLn "Running Harpe test suite..."
  runCoreTests
  runCompilerTests
  runParserTests
  putStrLn "\nAll tests passed successfully!"
  exitSuccess
