module Harpe.Parser where

import Control.Applicative
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Harpe.AST
import Harpe.Syntax
import Harpe.Parser.Combinators
import Harpe.Templates
  ( errValidationSeparateLine, errValidationSeparateCloser
  , errValidationUnbalanced, errValidationEventName
  , errParseUnexpectedEof, errParseWordBoundary, errParseFailed
  )

-- ---------------------------------------------------------------------------
-- Preprocessor
-- ---------------------------------------------------------------------------

-- | Split raw template text into (imports, declarations, body).
-- Walks lines top-to-bottom tracking a stack of open block types so that
-- only top-level Haskell declarations are extracted.
preprocess :: String -> ([String], [String], String)
preprocess input =
  let bodyLines = lines input

      isBlockOpenerLine trimmed =
        symOpen `isPrefixOf` trimmed
          && not (symClose `isInfixOf` trimmed)
          && firstWordAfterOpen trimmed `elem` blockOpeners

      firstWordAfterOpen trimmed =
        case words (drop symOpenLen trimmed) of
          (w:_) -> w
          []    -> ""

      processLine (imps, decls, bLines, stack) line =
        let trimmed  = trim line
            fw       = firstWordAfterOpen trimmed
            isOpener = isBlockOpenerLine trimmed
            isCloser = trimmed == symClose
        in if null stack
             then handleTopLevel imps decls bLines line trimmed fw isOpener
             else handleInLayout imps decls bLines stack line trimmed fw isOpener isCloser

      handleTopLevel imps decls bLines line trimmed fw isOpener
        | isOpener =
            (imps, decls, line : bLines, [fw])
        | symOpen `isPrefixOf` trimmed =
            if (symOpen ++ "{") `isPrefixOf` trimmed
              then (imps, drop symOpenLen trimmed : decls, "" : bLines, [])
              else (imps, decls, line : bLines, [])
        | "<" `isPrefixOf` trimmed =
            (imps, decls, line : bLines, [])
        | "import " `isPrefixOf` trimmed =
            (line : imps, decls, "" : bLines, [])
        | null trimmed =
            (imps, decls, "" : bLines, [])
        | otherwise =
            (imps, line : decls, "" : bLines, [])

      handleInLayout imps decls bLines stack line _trimmed fw isOpener isCloser =
        let nextStack
              | isCloser  = drop 1 stack
              | isOpener  = if fw == kwOn && stackTopIn onStackContext stack
                              then stack
                              else fw : stack
              | otherwise = stack
        in (imps, decls, line : bLines, nextStack)

      stackTopIn xs stk = case stk of { (s:_) -> s `elem` xs; [] -> False }

      hasOpener = any (isBlockOpenerLine . trim) bodyLines
      isHarpeStart = case bodyLines of
        (l:_) -> symOpen `isPrefixOf` trim l
        []    -> False
      initialStack = if hasOpener || isHarpeStart then [] else [kwRoot]

      (rawImps, rawDecls, processedBodyLines, _) =
        foldl processLine ([], [], [], initialStack) bodyLines

  in (reverse rawImps, reverse rawDecls, unlines (reverse processedBodyLines))

-- ---------------------------------------------------------------------------
-- Block boundary helpers
-- ---------------------------------------------------------------------------

-- | Returns True if the current parser position sits at a block boundary
-- (either a @=//@ closer or, when stopOnOn is True, a @//= on@ opener).
isStopTag :: Bool -> Parser Bool
isStopTag stopOnOn = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym        = drop symOpenLen trimmed
                afterSymTrimmed = dropWhile isSpace afterSym
            in if stopOnOn && case words afterSymTrimmed of { (w:_) -> w == kwOn; _ -> False }
                 then Right (True, s)
                 else Right (False, s)
       else if symClose `isPrefixOf` trimmed
              then Right (True, s)
              else Right (False, s)

-- | Returns True if the next token is a @//= on@ opener (without consuming).
isNextOnTag :: Parser Bool
isNextOnTag = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym        = drop symOpenLen trimmed
                afterSymTrimmed = dropWhile isSpace afterSym
            in Right (case words afterSymTrimmed of { (w:_) -> w == kwOn; _ -> False }, s)
       else Right (False, s)

-- | Returns True if the next token is a @//= in@ opener (without consuming).
isNextInTag :: Parser Bool
isNextInTag = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym = drop symOpenLen trimmed
                afterSymTrimmed = dropWhile isSpace afterSym
            in Right (case words afterSymTrimmed of { (w:_) -> w == kwIn; _ -> False }, s)
       else Right (False, s)

-- ---------------------------------------------------------------------------
-- Primitive helpers
-- ---------------------------------------------------------------------------

-- | Parse a keyword and assert a word boundary (no alphanum / _ / ' follows).
keyword :: String -> Parser String
keyword kw = do
  _ <- string kw
  Parser $ \s ->
    case s of
      (c:_) | isAlphaNum c || c == '_' || c == '\'' -> Left $ errParseWordBoundary kw
      _ -> Right (kw, s)

-- | Parse lines until the trimmed input starts with the given target string.
parseLinesUntil :: String -> Parser [String]
parseLinesUntil target = Parser $ \s ->
  if null s
    then Left $ errParseUnexpectedEof target
    else
      let trimmed = dropWhile isSpace s
      in if target `isPrefixOf` trimmed
           then Right ([], s)
           else case runParser parseLine s of
                  Left err  -> Left err
                  Right (line, rest) ->
                    case runParser (parseLinesUntil target) rest of
                      Left err         -> Left err
                      Right (ls, rest') -> Right (line : ls, rest')

-- ---------------------------------------------------------------------------
-- Block parsers
-- ---------------------------------------------------------------------------

isNextInOrCloseTag :: Parser (Maybe String)
isNextInOrCloseTag = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym = drop symOpenLen trimmed
                afterSymTrimmed = dropWhile isSpace afterSym
            in case words afterSymTrimmed of
                 (w:_) | w == kwIn -> Right (Just "in", s)
                 _                 -> Right (Nothing, s)
       else if symClose `isPrefixOf` trimmed
         then Right (Just "close", s)
         else Right (Nothing, s)

isNextInOrLetOrCloseTag :: Parser (Maybe String)
isNextInOrLetOrCloseTag = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym = drop symOpenLen trimmed
                afterSymTrimmed = dropWhile isSpace afterSym
            in case words afterSymTrimmed of
                 (w:_) | w == kwIn  -> Right (Just "in", s)
                       | w == kwLet -> Right (Just "let", s)
                 _                  -> Right (Nothing, s)
       else if symClose `isPrefixOf` trimmed
         then Right (Just "close", s)
         else Right (Nothing, s)

parseNodesUntilInOrLetOrClose :: Parser [TemplateNode]
parseNodesUntilInOrLetOrClose = do
  stop <- isNextInOrLetOrCloseTag
  case stop of
    Just _  -> pure []
    Nothing -> do
      n  <- parseNode
      ns <- parseNodesUntilInOrLetOrClose
      pure (n : ns)

parseOneLetBinding :: Parser (String, [String], [TemplateNode])
parseOneLetBinding = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwLet
  _ <- spaces1
  name <- identifier
  _ <- spaces
  args <- many (identifier <* spaces)
  _ <- optional (char '\n')
  defNodes <- parseNodesUntilInOrLetOrClose
  pure (name, args, defNodes)

parseLetBlock :: Parser TemplateNode
parseLetBlock = do
  bindings <- some parseOneLetBinding
  stop <- isNextInOrCloseTag
  case stop of
    Just "in" -> do
      _ <- spaces
      _ <- string symOpen
      _ <- spaces
      _ <- keyword kwIn
      _ <- spaces
      _ <- optional (char '\n')
      bodyNodes <- parseNodesUntil False
      _ <- spaces
      _ <- string symClose
      _ <- spaces
      _ <- optional (char '\n')
      pure $ LetBlock bindings bodyNodes
    Just "close" -> do
      _ <- spaces
      _ <- string symClose
      _ <- spaces
      _ <- optional (char '\n')
      pure $ LetDecl bindings
    _ -> empty

parseChunkBlock :: Parser TemplateNode
parseChunkBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwChunk
  _ <- spaces1
  name     <- identifier
  _ <- spaces
  args     <- many (identifier <* spaces)
  _ <- optional (char '\n')
  bodyNodes <- parseNodesUntil False
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ ChunkBlock name args bodyNodes

parseRootBlock :: Parser TemplateNode
parseRootBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwRoot
  _ <- spaces
  _ <- optional (char '\n')
  bodyNodes <- parseNodesUntil False
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ RootBlock bodyNodes

parseDefaultBlock :: Parser TemplateNode
parseDefaultBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwDefault
  mbOn <- optional (hspaces *> char '/' *> hspaces)
  defaultPatterns <- case mbOn of
    Just _ -> do
      patLine <- parseLine
      pure (map (stripPrefixOn . trim) (splitOn '/' patLine))
    Nothing -> do
      _ <- hspaces
      _ <- optional (char '\n')
      pure []
  defaultNodes <- parseNodesUntil True
  onBlocks     <- parseOnBlocks
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ DefaultBlock defaultPatterns defaultNodes onBlocks

parseStandaloneOnBlocks :: Parser TemplateNode
parseStandaloneOnBlocks = do
  onBlocks <- parseOnBlocks1
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ DefaultBlock [] [] onBlocks

parseOnBlocks1 :: Parser [TemplateNode]
parseOnBlocks1 = do
  b  <- parseOnBlock
  bs <- parseOnBlocks
  pure (b : bs)

parseOnBlocks :: Parser [TemplateNode]
parseOnBlocks = do
  nextIsOn <- isNextOnTag
  if nextIsOn
    then do
      b  <- parseOnBlock
      bs <- parseOnBlocks
      pure (b : bs)
    else pure []

parseOnBlock :: Parser TemplateNode
parseOnBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwOn
  _ <- spaces
  _ <- optional (char '$' *> spaces)
  patternStr <- parseLine
  bodyNodes  <- parseNodesUntil True
  pure $ OnBlock patternStr bodyNodes

parseCrudeBlock :: Parser TemplateNode
parseCrudeBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- hspaces
  _ <- keyword kwCrude
  _ <- hspaces
  mbName <- optional identifier
  args   <- case mbName of
    Just _ -> many (hspaces1 *> identifier)
    Nothing -> pure []
  _ <- hspaces
  _ <- optional (char '\r')
  _ <- char '\n'
  bodyLines <- parseLinesUntil symClose
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ CrudeBlock mbName args bodyLines

-- | Parse an alien body: read lines until @=//@ or a @//= tinkle@ separator.
-- Returns (body lines, optional tinkle body lines).
parseAlienBody :: Parser ([String], Maybe [String])
parseAlienBody = Parser $ \s -> go [] s
  where
    go acc s' =
      let trimmed = dropWhile isSpace s'
      in if symClose `isPrefixOf` trimmed
           then Right ((reverse acc, Nothing), s')
           else
             let lineEnd = break (== '\n') s'
                 (lineStr, afterNewline) = lineEnd
                 lineTrimmed = trim lineStr
             in case words lineTrimmed of
                  ["//=", "tinkle"] ->
                    let restToParse = case afterNewline of { ('\n':r) -> r; other -> other }
                    in case runParser (parseLinesUntil symClose) restToParse of
                         Left err -> Left err
                         Right (tinkleLines, rest) -> Right ((reverse acc, Just tinkleLines), rest)
                  _ -> case runParser parseLine s' of
                         Left err -> Left err
                         Right (line, rest) -> go (line : acc) rest

parseAlienBlock :: Parser TemplateNode
parseAlienBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- hspaces
  _ <- keyword kwAlien
  _ <- hspaces1
  name      <- identifier
  args      <- many (hspaces1 *> identifier)
  _ <- hspaces
  _ <- optional (char '\r')
  _ <- char '\n'
  (bodyLines, mbTinkle) <- parseAlienBody
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ AlienBlock name args bodyLines mbTinkle


parsePropagatedAlienBlock :: Parser TemplateNode
parsePropagatedAlienBlock = do
  _ <- spaces
  _ <- string symOpen
  _ <- hspaces
  _ <- keyword kwPropagate
  _ <- hspaces1
  _ <- keyword kwAlien
  _ <- hspaces1
  name      <- identifier
  args      <- many (hspaces1 *> identifier)
  _ <- hspaces
  _ <- optional (char '\r')
  _ <- char '\n'
  (bodyLines, mbTinkle) <- parseAlienBody
  _ <- spaces
  _ <- string symClose
  _ <- spaces
  _ <- optional (char '\n')
  pure $ PropagatedAlienBlock name args bodyLines mbTinkle



parseAddAlien :: Parser TemplateNode
parseAddAlien = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwAdd
  _ <- spaces1
  _ <- string kwAlien
  _ <- spaces1
  className <- identifier
  _ <- spaces1
  mbAs <- optional (string "as" <* spaces1)
  instName <- case mbAs of
                Just _  -> identifier <* spaces1
                Nothing -> pure className
  _ <- string kwFrom
  _ <- spaces1
  line <- parseLine
  let path = stripInlineClose line
  pure $ AddAlien className instName path

parsePropagateAlien :: Parser TemplateNode
parsePropagateAlien = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwPropagate
  _ <- spaces1
  _ <- string kwAlien
  _ <- spaces1
  line <- parseLine
  if symClose `isInfixOf` line
    then let (n, _) = breakString symClose line in pure $ PropagateAlien (trim n)
    else empty



parseImplyCrude :: Parser TemplateNode
parseImplyCrude = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwImply
  _ <- spaces1
  _ <- string kwCrude
  _ <- spaces1
  line <- parseLine
  let rest = stripInlineClose line
      ws   = words rest
  case ws of
    []          -> empty
    (name:args) -> pure $ ImplyCrude name args

parseImplyAlien :: Parser TemplateNode
parseImplyAlien = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwImply
  _ <- spaces1
  _ <- string kwAlien
  _ <- spaces1
  line <- parseLine
  let rest = stripInlineClose line
      ws   = words rest
  case ws of
    []          -> empty
    (name:args) -> pure $ ImplyAlien name args



parseImply :: Parser TemplateNode
parseImply = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwImply
  _ <- spaces1
  line <- parseLine
  let expr = stripInlineClose line
  pure $ HaskellExpr (kwImply ++ " " ++ expr)

parseWithoutDirective :: Parser TemplateNode
parseWithoutDirective = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- string kwWithout
  _ <- spaces1
  line <- parseLine
  let rest  = stripInlineClose (stripAltClose line)
      clean = map (\c -> if c == '/' then ' ' else c) rest
      parts = words clean
  if kwMvu `elem` parts
       || (all (`elem` ["model","update","view"]) parts && length parts == 3)
    then pure WithoutMVU
    else empty
  where
    stripAltClose s
      | symAltClose `isInfixOf` s = let (r, _) = breakString symAltClose s in r
      | otherwise                  = s

-- | Inline Haskell declaration: @//= someExpr@ with no @=//@ on the same line
-- and not starting with a block keyword.
parseHaskellDecl :: Parser TemplateNode
parseHaskellDecl = Parser $ \s ->
  let trimmed = dropWhile isSpace s
  in if symOpen `isPrefixOf` trimmed
       then let afterSym      = drop symOpenLen trimmed
                (line, rest)  = break (== '\n') afterSym
            in if symClose `isInfixOf` line
                 then Left "Has inline transition"
                 else let fw = case words (dropWhile isSpace line) of { (x:_) -> x; [] -> "" }
                      in if fw `elem` blockKeywords
                    then Left "Is a block tag or template directive"
                   else
                     let rest' = case rest of
                                   ('\n':r) -> r
                                   r        -> r
                     in Right (HaskellDecl (trim line), rest')
       else Left "Not a Haskell declaration"

parseClinchTemplate :: Parser TemplateNode
parseClinchTemplate = do
  _ <- spaces
  _ <- string symOpen
  _ <- spaces
  _ <- keyword kwClinch
  _ <- spaces1
  line <- parseLine
  let cleaned = stripInlineClose line
      ws      = words cleaned
  case ws of
    [path]          -> pure $ ClinchTemplate path Nothing
    [path, handler] -> pure $ ClinchTemplate path (Just handler)
    _               -> empty

parseEventBinding :: Parser TemplateNode
parseEventBinding = do
  _ <- string symOpen
  _ <- spaces
  evtName <- identifier
  if isEventName evtName
    then do
      _ <- spaces
      argsStr <- parseUntilStringWithout "\n" symClose
      _ <- string symClose
      pure $ EventBinding evtName (words argsStr)
    else empty

parseInlineExpr :: Parser TemplateNode
parseInlineExpr = do
  _ <- string symOpen
  expr <- parseUntilStringWithout "\n" symClose
  _ <- string symClose
  pure $ HaskellExpr (trim expr)

parseHtmlChunk :: Parser TemplateNode
parseHtmlChunk = Parser $ \s ->
  if null s
    then Left "EOF"
    else
      let go [] acc        = (reverse acc, [])
          go s'@(c:cs) acc
            | symOpen  `isPrefixOf` s' = (reverse acc, s')
            | symClose `isPrefixOf` s' = (reverse acc, s')
            | otherwise                = go cs (c : acc)
          (chunk, rest) = go s []
      in if null chunk
           then Left "Expected HtmlChunk"
           else Right (HtmlChunk chunk, rest)

-- ---------------------------------------------------------------------------
-- Node dispatcher
-- ---------------------------------------------------------------------------

parseNodesUntil :: Bool -> Parser [TemplateNode]
parseNodesUntil stopOnOn = do
  stop <- isStopTag stopOnOn
  if stop
    then pure []
    else do
      n  <- parseNode
      ns <- parseNodesUntil stopOnOn
      pure (n : ns)

parseNode :: Parser TemplateNode
parseNode = parseLetBlock
        <|> parseChunkBlock
        <|> parseRootBlock
        <|> parseDefaultBlock
        <|> parseStandaloneOnBlocks
        <|> parseCrudeBlock
        <|> parseAlienBlock
        <|> parsePropagatedAlienBlock
        <|> parseAddAlien
        <|> parsePropagateAlien
        <|> parseImplyAlien
        <|> parseImplyCrude
        <|> parseImply
        <|> parseWithoutDirective
        <|> parseHaskellDecl
        <|> parseClinchTemplate
        <|> parseEventBinding
        <|> parseInlineExpr
        <|> parseHtmlChunk

parseNodes :: Parser [TemplateNode]
parseNodes = many parseNode

-- ---------------------------------------------------------------------------
-- Validation (runs before parsing)
-- ---------------------------------------------------------------------------

validateTemplateText :: String -> Either String ()
validateTemplateText input = do
  checkSeparateLines input
  checkBalancedBlocks input
  checkEventNames input
  checkLegacyDirectives input

-- | E005 — legacy control directives (if/else/endif) are rejected.
checkLegacyDirectives :: String -> Either String ()
checkLegacyDirectives input =
  let ls = zip ([1..] :: [Int]) (lines input)
      checkLine (lineNo, line) =
        let t = trim line
        in if any (`isPrefixOf` t) ["//= if ", "//= endif", "//= else"]
             then Left $ "harpe E005 — line " ++ show lineNo ++ ": Legacy control directives are no longer supported. Use '//= default' and '//= on' instead."
             else Right ()
  in mapM_ checkLine ls

-- | E001/E002 — block openers and closers must each occupy their own line.
checkSeparateLines :: String -> Either String ()
checkSeparateLines input =
  let ls = zip ([1..] :: [Int]) (lines input)
      checkLine (lineNo, line) =
        let t = trim line
            hasOpener =
              any (`isInfixOf` t)
                [ symOpen ++ " " ++ kwDefault
                , symOpen ++ " " ++ kwOn ++ " "
                , symOpen ++ " " ++ kwChunk ++ " "
                , symOpen ++ " " ++ kwCrude
                , symOpen ++ " " ++ kwAlien
                , symOpen ++ " " ++ kwLet ++ " "
                ]
              || (symOpen ++ " " ++ kwIn ++ " ") `isInfixOf` (t ++ " ")
              || ( any (`isInfixOf` t)
                     [ symOpen ++ " " ++ kwPropagate ++ " " ++ kwAlien ]
                   && not (symClose `isInfixOf` t) )
        in if hasOpener
             then if not (symOpen `isPrefixOf` t)
                       || symClose `isInfixOf` t
                       || "<" `isInfixOf` t
                       || ">" `isInfixOf` t
                    then Left (errValidationSeparateLine lineNo)
                    else Right ()
             else if symClose `isInfixOf` t && not (symOpen `isInfixOf` t)
                    then if t /= symClose
                           then Left (errValidationSeparateCloser lineNo)
                           else Right ()
                    else Right ()
  in mapM_ checkLine ls

-- | E003 — every block opener must have a matching closer.
checkBalancedBlocks :: String -> Either String ()
checkBalancedBlocks input =
  let ls = lines input

      processStack stack line =
        let trimmed  = trim line
            fw       = case words (drop symOpenLen trimmed) of { (w:_) -> w; [] -> "" }
            isOpener = symOpen `isPrefixOf` trimmed
                         && not (symClose `isInfixOf` trimmed)
                         && fw `elem` blockOpeners
            isCloser = trimmed == symClose
        in if null stack
             then if isOpener then [fw] else []
             else if isCloser
               then drop 1 stack
               else if isOpener
                 then if fw == kwOn && stackTopIn onStackContext stack
                        then stack
                        else fw : stack
                 else stack

      stackTopIn xs stk = case stk of { (s:_) -> s `elem` xs; [] -> False }
      finalStack = foldl processStack [] ls
  in if null finalStack
       then Right ()
       else Left errValidationUnbalanced

-- | E004 — @onXxx@ attribute names must be valid camelCase event names.
checkEventNames :: String -> Either String ()
checkEventNames input =
  let findInline [] = []
      findInline s  =
        case breakString symOpen s of
          (_, "") -> []
          (_, rest) ->
            case breakString symClose (drop symOpenLen rest) of
              ("", _)               -> []
              (inside, remaining)   -> inside : findInline (drop symOpenLen remaining)

      inlines   = findInline input
      checkWord inside =
        case words inside of
          (w:_) | kwOn `isPrefixOf` w && length w > length kwOn ->
            if isEventName w then Right () else Left (errValidationEventName w)
          _ -> Right ()
  in mapM_ checkWord inlines

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

parseTemplate :: String -> Either String TemplateAST
parseTemplate input = do
  validateTemplateText input
  let (imps, decls, body) = preprocess input
  case runParser (parseNodes <* eof) body of
    Left err        -> Left (errParseFailed err)
    Right (nodes, _) -> Right $ TemplateAST imps decls nodes

-- ---------------------------------------------------------------------------
-- Internal utilities
-- ---------------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn d xs =
  let (val, rest) = break (== d) xs
  in val : case rest of
             []     -> []
             (_:ys) -> splitOn d ys

-- | Strip a trailing @=//@ (or inline close) from a parsed line.
stripInlineClose :: String -> String
stripInlineClose line
  | symClose `isInfixOf` line = let (r, _) = breakString symClose line in trim r
  | otherwise                 = trim line
-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

stripPrefixOn :: String -> String
stripPrefixOn s =
  if "on " `isPrefixOf` s
     then trim (drop 3 s)
     else s
