-- | Harpe.Compiler.PatternValidator — on-block pattern validation.
--
-- Validates patterns like @ValueChanged val@ or @RedirectTo "info" / Done@
-- that appear in @//= on …@ directives.
module Harpe.Compiler.PatternValidator
  ( validatePattern
  , splitRespectingParens
  , wrapPatternForChild
  , stripDollarPrefix
  ) where

import Data.Char (isSpace, isAlphaNum, isUpper, isLower, isDigit)

import Harpe.Parser.Combinators (trim, breakString)
import Harpe.CodeGen.Names (nmEventParent, mkEventCtor)
import Harpe.Templates (errPatternArg, errPatternInvalidCtor, errPatternEmpty)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Validate a single pattern string (may contain @/@ for multi-patterns).
-- Returns @Left err@ if any sub-pattern is malformed.
validatePattern :: String -> Either String ()
validatePattern rawPat = do
  let cleanPat = trim rawPat
  case parsePatTokens cleanPat of
    Left _          -> Left errPatternEmpty
    Right (ctor, args) ->
      if ctor == "flick" && null args
        then Right ()
      else if not (isCtor ctor)
        then Left (errPatternInvalidCtor rawPat)
        else mapM_ validateArg args

-- | Wrap a pattern for use in a child-event case clause.
-- Prefixes with @EventParent (…)@ unless the pattern already names a known
-- child-event constructor or starts with @(@.
wrapPatternForChild :: [String] -> String -> String
wrapPatternForChild childModules rawPat =
  let p = stripDollarPrefix rawPat
      tokens = splitRespectingParens p
      firstWord = case tokens of { (w:_) -> w; [] -> "" }
      knownCtors = nmEventParent : map mkEventCtor childModules
      isAlreadyWrapped =
        firstWord `elem` knownCtors || firstWord == "_" || firstWord == "flick" || case firstWord of { ('(':_) -> True; _ -> False }
  in if isAlreadyWrapped then (if p == "flick" then "_" else p) else nmEventParent ++ " (" ++ p ++ ")"

-- | Strip a leading @$@ sigil and surrounding whitespace from a pattern.
stripDollarPrefix :: String -> String
stripDollarPrefix pat =
  let t = trim pat
  in case t of
       ('$':rest) -> trim rest
       _          -> t

-- | Split a string on whitespace while respecting parenthesis nesting depth.
splitRespectingParens :: String -> [String]
splitRespectingParens s = go s (0 :: Int) "" []
  where
    go [] _ ""  acc = reverse acc
    go [] _ cur acc = reverse (reverse cur : acc)
    go (c:cs) depth cur acc
      | c == '('             = go cs (depth + 1) (c : cur) acc
      | c == ')'             = go cs (depth - 1) (c : cur) acc
      | isSpace c && depth == 0 =
          if null cur
            then go cs depth "" acc
            else go cs depth "" (reverse cur : acc)
      | otherwise            = go cs depth (c : cur) acc

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

parsePatTokens :: String -> Either String (String, [String])
parsePatTokens s =
  case splitRespectingParens s of
    []          -> Left "Empty pattern"
    (ctor:args) -> Right (ctor, args)

validateArg :: String -> Either String ()
validateArg arg =
  let cleanArg = removeParens arg
  in if cleanArg == "_"
       then Right ()
       else if isStringLiteral cleanArg || isNumericLiteral cleanArg || isBoolLiteral cleanArg
              then Right ()
              else if isVarWithOrWithoutType cleanArg
                     then Right ()
                     else case parsePatTokens cleanArg of
                            Right (nestedCtor, nestedArgs) | isCtor nestedCtor ->
                              mapM_ validateArg nestedArgs
                            _ -> Left (errPatternArg arg arg)

isCtor :: String -> Bool
isCtor s =
  let s' = removeParens s
  in case s' of
       []    -> False
       (c:_) -> isUpper c || c == '('

removeParens :: String -> String
removeParens s =
  let s' = trim s
  in case (s', reverse s') of
       ('(':_, ')':_) | length s' >= 2 -> drop 1 (take (length s' - 1) s')
       _                               -> s'

isStringLiteral :: String -> Bool
isStringLiteral s = case (s, reverse s) of
  ('"':_, '"':_) | length s >= 2 -> True
  _                               -> False

isNumericLiteral :: String -> Bool
isNumericLiteral s = not (null s) && all isDigit s

isBoolLiteral :: String -> Bool
isBoolLiteral s = s `elem` ["True", "False"]

isVarWithOrWithoutType :: String -> Bool
isVarWithOrWithoutType s =
  case breakString "::" s of
    (varPart, "")       -> isVarName (trim varPart)
    (varPart, typePart) -> isVarName (trim varPart) && isTypeName (trim (drop 2 typePart))

isVarName :: String -> Bool
isVarName s = case s of
  []    -> False
  (c:r) -> isLower c && all (\x -> isAlphaNum x || x `elem` "_'") r

isTypeName :: String -> Bool
isTypeName s = case s of
  []    -> False
  (c:r) -> isUpper c && all (\x -> isAlphaNum x || x `elem` "_'") r
