module TestHelper where

import System.Exit (exitFailure)
import Data.List (isInfixOf)
import Control.Exception (catch, SomeException)

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual name expectedActual actual =
  if expectedActual == actual
    then putStrLn $ "  [OK] " ++ name
    else do
      putStrLn $ "  [FAIL] " ++ name
      putStrLn $ "    Expected: " ++ show expectedActual
      putStrLn $ "    Actual:   " ++ show actual
      exitFailure

assertLeft :: Show a => String -> String -> Either String a -> IO ()
assertLeft name expectedSubstr actual =
  case actual of
    Left err ->
      if expectedSubstr `isInfixOf` err
        then putStrLn $ "  [OK] " ++ name
        else do
          putStrLn $ "  [FAIL] " ++ name
          putStrLn $ "    Expected substring: " ++ show expectedSubstr
          putStrLn $ "    Actual error:       " ++ show err
          exitFailure
    Right val -> do
      putStrLn $ "  [FAIL] " ++ name
      putStrLn $ "    Expected error with substring: " ++ show expectedSubstr
      putStrLn $ "    But parsed successfully to: " ++ show val
      exitFailure

assertThrowsIO :: String -> String -> IO a -> IO ()
assertThrowsIO name expectedSubstr action = do
  res <- catch (action >> pure (Right ())) (\e -> pure (Left (show (e :: SomeException))))
  case res of
    Left err ->
      if expectedSubstr `isInfixOf` err
        then putStrLn $ "  [OK] " ++ name
        else do
          putStrLn $ "  [FAIL] " ++ name
          putStrLn $ "    Expected exception containing: " ++ show expectedSubstr
          putStrLn $ "    Actual exception:              " ++ show err
          exitFailure
    Right _ -> do
      putStrLn $ "  [FAIL] " ++ name
      putStrLn $ "    Expected exception containing: " ++ show expectedSubstr
      putStrLn $ "    But it completed successfully."
      exitFailure
