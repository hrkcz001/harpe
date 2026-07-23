module Harpe.Compiler.Common where

import Data.Char (toLower, isAlphaNum)
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)
import System.IO (withFile, IOMode(..), hSetEncoding, utf8, hGetContents)
import Harpe.AST
import Harpe.CodeGen.Names

-- | Read a file with UTF-8 encoding, forcing full evaluation before close.
readUtf8File :: FilePath -> IO String
readUtf8File path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  content <- hGetContents h
  mapM_ (\c -> c `seq` pure ()) content
  pure content

-- | Lower-case the first character of a string.
toLowerFirst :: String -> String
toLowerFirst []     = []
toLowerFirst (x:xs) = toLower x : xs

-- | Derive an IncludeInfo from a relative @.harpe@ path and optional handler.
-- E.g. @"Counter.harpe" -> IncludeInfo "Counter" "counter" Nothing@.
includeInfoFromPath :: FilePath -> Maybe String -> IncludeInfo
includeInfoFromPath relPath mbHandler =
  let modName = takeBaseName relPath
  in IncludeInfo modName (mkFieldName modName) mbHandler

-- | Rename bare @Msg@/@Model@/@Event@ to the @Parent*@ prefixed versions
-- at word boundaries (so @ModelState@ is left untouched).
replaceWordBoundaries :: String -> String
replaceWordBoundaries [] = []
replaceWordBoundaries s@(c:cs)
  | isAlphaNum c =
      let (word, rest) = span isAlphaNum s
          replaced = case word of
                       "Msg"   -> nmParentMsg
                       "Model" -> nmParentModel
                       "Event" -> nmParentEvent
                       other   -> other
      in replaced ++ replaceWordBoundaries rest
  | otherwise = c : replaceWordBoundaries cs

-- | Replace special characters with spaces (used before word-level checks).
replaceSpecialChars :: String -> String
replaceSpecialChars = map (\c -> if isAlphaNum c || c `elem` "_'" then c else ' ')

-- | Replace all non-overlapping occurrences of @old@ with @new@ in @s@.
replaceSubstring :: String -> String -> String -> String
replaceSubstring _ _ [] = []
replaceSubstring old new s@(c:cs)
  | old `isPrefixOf` s = new ++ replaceSubstring old new (drop (length old) s)
  | otherwise          = c   : replaceSubstring old new cs
