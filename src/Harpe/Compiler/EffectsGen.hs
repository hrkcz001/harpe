-- | Harpe.Compiler.EffectsGen — effects-pass code generation.
--
-- Compiles @[TemplateNode]@ lists into @IO ()@ expressions for the
-- generated @effects@ method of a Component instance.
module Harpe.Compiler.EffectsGen
  ( compileNodesWithDeclsEffects
  , compileNode
  , extractContextualAlienIds
  , ctxAccumFetch
  ) where

import Data.List (intercalate, isPrefixOf, group, sort)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.IORef (IORef, readIORef, writeIORef)

import Harpe.AST
import Harpe.CodeGen.Names
import Harpe.Compiler.Common (includeInfoFromPath)
import Harpe.Compiler.NodeUtils (processCrudeLine, splitOnChar)
import Harpe.Compiler.PatternValidator (validatePattern, wrapPatternForChild, stripDollarPrefix)
import Harpe.Parser.Combinators (trim)
import Harpe.Templates
  ( errInternalAlienNode
  , errImplyTransformOutsideCrude
  , errCrudeInvalidContext
  )

-- ---------------------------------------------------------------------------
-- Contextual alien ID extraction
-- ---------------------------------------------------------------------------

-- | Recursively scan template nodes for 'imply contextual alien <name>' lines
-- inside crude blocks, returning the list of unique alien/informer names.
-- | Unique elements after sorting (safer than `map head (group (sort xs))`).
uniqueSorted :: Ord a => [a] -> [a]
uniqueSorted = mapMaybe listToMaybe . group . sort

extractContextualAlienIds :: [TemplateNode] -> [String]
extractContextualAlienIds nodes =
  uniqueSorted (concatMap goNode nodes)
  where
    goNode (CrudeBlock _ _ bodyLines) = concatMap extractFromLine bodyLines
    goNode (DefaultBlock _ ds os)    = extractContextualAlienIds ds ++ extractContextualAlienIds os
    goNode (OnBlock _ body)          = extractContextualAlienIds body
    goNode (LetBlock bindings body)   = concatMap (\(_,_,def) -> extractContextualAlienIds def) bindings ++ extractContextualAlienIds body
    goNode (LetDecl bindings)         = concatMap (\(_,_,def) -> extractContextualAlienIds def) bindings
    goNode (RootBlock body)          = extractContextualAlienIds body
    goNode (ChunkBlock _ _ body)     = extractContextualAlienIds body
    goNode _                         = []

    extractFromLine line =
      let wds = words line
      in case wds of
           ("imply":"contextual":"alien":name:_) -> [name]
           _ -> []

-- | Read all accumulated IORef declaration strings from the accumulator.
ctxAccumFetch :: IORef [String] -> IO [String]
ctxAccumFetch = readIORef

-- | Generate a unique IORef name for tracking contextual informer state.
mkCtxIORefName :: String -> Int -> String
mkCtxIORefName modName idx = "ctxTracker_" ++ modName ++ "_" ++ show idx

-- | Generate the top-level IORef declaration string.
genCtxIORefDecl :: String -> Int -> [String] -> String
genCtxIORefDecl modName idx initialIds =
  let name = mkCtxIORefName modName idx
  in name ++ " :: IORef [T.Text]\n" ++
     name ++ " = unsafePerformIO (newIORef " ++ show initialIds ++ ")\n" ++
     "{-# NOINLINE " ++ name ++ " #-}"

-- | Used internally as a counter for ctx IORefs (separate from blockRef).
-- This is called during DefaultBlock compilation to get a unique index.
getNextCtxIdx :: IORef Int -> IO Int
getNextCtxIdx ref = do
  val <- readIORef ref
  writeIORef ref (val + 1)
  pure val

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Compile a node sequence into an @IO ()@ expression, threading
-- @HaskellDecl@ nodes as @let … in@ wrappers.
-- The 'IORef [String]' accumulates top-level IORef declarations for ctx tracking
-- (used by parent DefaultBlocks).
-- 'tinkleNames' is the set of alien names that have a tinkle cleanup.
compileNodesWithDeclsEffects
  :: IORef Int -> IORef [String] -> IORef Int -> String -> FilePath -> [String] -> [String]
  -> [String] -> [TemplateNode] -> IO String
compileNodesWithDeclsEffects _ _ _ _ _ _ _ _ [] = pure "return ()"
compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames nodes = do
  compileDecls nodes
  where
    compileDecls ns = case break isDecl ns of
      (nonDecls, []) ->
        compileSimpleNodes blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames nonDecls
      (nonDecls, HaskellDecl decl : rest) -> do
        prefix <- if null nonDecls
                    then pure ""
                    else do
                      s <- compileSimpleNodes blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames nonDecls
                      pure $ if s == "return ()" then "" else s ++ " >>\n        "
        restExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames rest
        let cleanDecl = if "let " `isPrefixOf` decl then decl else "let " ++ decl
        pure $ prefix ++ "(" ++ cleanDecl ++ " in (" ++ restExpr ++ "))"
      (nonDecls, LetDecl bindings : rest) -> do
        prefix <- if null nonDecls
                    then pure ""
                    else do
                      s <- compileSimpleNodes blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames nonDecls
                      pure $ if s == "return ()" then "" else s ++ " >>\n        "
        compiledBindings <- mapM (\(bName, bArgs, bDef) -> do
          defStr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames bDef
          let defE = if null defStr || defStr == "return ()" then "(return () :: IO ())" else "((" ++ defStr ++ ") :: IO ())"
          let argsStr = if null bArgs then "" else " " ++ unwords bArgs
          pure $ bName ++ argsStr ++ " = " ++ defE) bindings
        restExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames rest
        let letStr = "let { " ++ intercalate "; " compiledBindings ++ " } in (" ++ restExpr ++ ")"
        pure $ prefix ++ "(" ++ letStr ++ ")"
      _ -> error "Should not happen"

    isDecl (HaskellDecl _) = True
    isDecl (LetDecl _)     = True
    isDecl _               = False



-- ---------------------------------------------------------------------------
-- Internal: node-level compilation
-- ---------------------------------------------------------------------------

compileSimpleNodes
  :: IORef Int -> IORef [String] -> IORef Int -> String -> FilePath -> [String] -> [String]
  -> [String] -> [TemplateNode] -> IO String
compileSimpleNodes _ _ _ _ _ _ _ _ [] = pure "return ()"
compileSimpleNodes blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames ns = do
  compiled <- mapM (compileNode blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames) ns
  let meaningful = filter (not . null) compiled
  pure $ if null meaningful then "return ()" else intercalate " >>\n        " meaningful

compileNode
  :: IORef Int -> IORef [String] -> IORef Int -> String -> FilePath -> [String] -> [String]
  -> [String] -> TemplateNode -> IO String

-- HTML/expression nodes have no effects — emit empty string (filtered out by compileSimpleNodes)
compileNode _ _ _ _ _ _ _ _ (HtmlChunk _)        = pure ""
compileNode _ _ _ _ _ _ _ _ (HaskellExpr _)      = pure ""
compileNode _ _ _ _ _ _ _ _ (EventBinding _ _)   = pure ""

-- Clinch: delegate to child component effects
compileNode _ _ _ _ _ _ _ _ (ClinchTemplate relPath mbHandler) =
  let info = includeInfoFromPath relPath mbHandler
      modN  = includeModuleName info
      fldN  = includeFieldName info
  in pure $
       "(Harpe.Core.effects (" ++ rtMsgWrappers ++ " ++ [" ++ show (mkMsgCtor modN) ++ "]) "
       ++ "(" ++ fldN ++ "Model " ++ rtComposedModel ++ ") "
       ++ "(case " ++ rtActiveEvent ++ " of { Just (" ++ mkEventCtor modN ++ " e) -> Just e; _ -> Nothing }))"

-- DefaultBlock: run effects for whichever on-branch is active
compileNode blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames (DefaultBlock defaultPatterns defNodes onBlocks) = do
  defExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames defNodes
  -- Collect contextual alien IDs from BOTH defNodes and all onBlocks (recursively)
  let allCtxIds = extractContextualAlienIds defNodes
                  ++ extractContextualAlienIds (concatMap onBlockNodes onBlocks)
      allCtxIdsNub = uniqueSorted allCtxIds
      onBlockNodes (OnBlock _ nodes) = nodes
      onBlockNodes _ = []
  if null allCtxIdsNub
    then do
      -- No contextual aliens — use the old API
      onExprs <- mapM (compileOnBlock blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames) onBlocks
      let defPats = concatMap
            (\pat -> nmEventParent ++ " (" ++ stripDollarPrefix pat ++ ") -> Just (return ()); ")
            defaultPatterns
          effectsCall = "default_effects ( \\ mb -> on_effects ( \\ event -> case event of { "
                        ++ concat onExprs ++ defPats ++ "_ -> Nothing }) mb) " ++ rtActiveEvent
      pure $ if null defExpr || defExpr == "return ()"
               then effectsCall
               else defExpr ++ " >>\n        " ++ effectsCall
    else do
      -- Has contextual aliens — generate IORef tracking and tinkle cleanup
      ctxIdx <- getNextCtxIdx ctxRef
      let defCtxIds = extractContextualAlienIds defNodes
          iorefName = mkCtxIORefName modName ctxIdx
          iorefDecl = genCtxIORefDecl modName ctxIdx defCtxIds
          -- Only include aliens that actually have a tinkle
          tinkleIds = filter (`elem` tinkleNames) allCtxIdsNub
          tinkleLambda = "\\name -> case name of { " ++
            concatMap (\n -> show n ++ " -> " ++ n ++ nmTinkleSuffix ++ "; ") tinkleIds ++
            "_ -> return () }"
      -- Record the IORef declaration in the accumulator
      writeIORef ctxAccum . (iorefDecl :) =<< readIORef ctxAccum
      -- Build on-block clauses with ([ids], io) tuples using compileOnBlockCtx
      onExprsCtx <- mapM (compileOnBlockCtx blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames) onBlocks
      let defPats = concatMap
            (\pat -> nmEventParent ++ " (" ++ stripDollarPrefix pat ++ ") -> Just (" ++ show defCtxIds ++ ", return ()); ")
            defaultPatterns
          effectsCall = "default_effects_ctx " ++ iorefName ++ " (" ++ tinkleLambda ++ ") ( \\ mb -> on_effects_ctx ( \\ event -> case event of { "
                        ++ concat onExprsCtx ++ defPats ++ "_ -> Nothing }) mb) " ++ rtActiveEvent
      pure $ if null defExpr || defExpr == "return ()"
               then effectsCall
               else defExpr ++ " >>\n        " ++ effectsCall

-- Anonymous crude block: compile body lines as sequenced IO actions
compileNode _ _ _ _ _ _ alienNames _ (CrudeBlock Nothing [] bodyLines) =
  case mapM (processCrudeLine alienNames) bodyLines of
    Left err            -> fail err
    Right processedLines -> pure (intercalate " >>\n        " processedLines)

-- Named / parameterised crude blocks are top-level declarations, not body nodes
compileNode _ _ _ _ _ _ _ _ (CrudeBlock mbName _ _) =
  ioError (userError (errCrudeInvalidContext (maybe "anonymous" id mbName)))

-- ImplyCrude: direct function call
compileNode _ _ _ _ _ _ _ _ (ImplyCrude name args) =
  pure (unwords (name : args))

-- Alien / informer nodes should never appear as body nodes
compileNode _ _ _ _ _ _ _ _ (AlienBlock _ _ _ _)           = ioError (userError (errInternalAlienNode "AlienBlock"))
compileNode _ _ _ _ _ _ _ _ (AddAlien _ _ _)               = ioError (userError (errInternalAlienNode "AddAlien"))
compileNode _ _ _ _ _ _ _ _ (PropagateAlien _)           = ioError (userError (errInternalAlienNode "PropagateAlien"))
compileNode _ _ _ _ _ _ _ _ (PropagatedAlienBlock _ _ _ _) = ioError (userError (errInternalAlienNode "PropagatedAlienBlock"))
compileNode _ _ _ _ _ _ _ _ (ImplyAlien name args)       = ioError (userError (errImplyTransformOutsideCrude name args))

-- These produce no effects
compileNode _ _ _ _ _ _ _ _ WithoutMVU                       = pure ""

-- These should never reach compileNode (partitioned / threaded before this point)
compileNode _ _ _ _ _ _ _ _ (OnBlock _ _)      = pure ""
compileNode _ _ _ _ _ _ _ _ (HaskellDecl _)   = pure ""
compileNode _ _ _ _ _ _ _ _ (LetDecl _)       = pure ""
compileNode _ _ _ _ _ _ _ _ (ChunkBlock _ _ _) = pure ""
compileNode _ _ _ _ _ _ _ _ (RootBlock _)     = pure ""
compileNode blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames (LetBlock bindings bodyNodes) = do
  compiledBindings <- mapM (\(bName, bArgs, bDef) -> do
    defExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames bDef
    let defE = if null defExpr || defExpr == "return ()" then "(return () :: IO ())" else "((" ++ defExpr ++ ") :: IO ())"
    let argsStr = if null bArgs then "" else " " ++ unwords bArgs
    pure $ bName ++ argsStr ++ " = " ++ defE) bindings
  bodyExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName srcDir childMods alienNames tinkleNames bodyNodes
  let bodyE = if null bodyExpr || bodyExpr == "return ()" then "(return () :: IO ())" else "((" ++ bodyExpr ++ ") :: IO ())"
  pure $ "let { " ++ intercalate "; " compiledBindings ++ " } in " ++ bodyE

-- ---------------------------------------------------------------------------
-- Internal: on-block compilation (standard, returns Just (IO ()))
-- ---------------------------------------------------------------------------

compileOnBlock
  :: IORef Int -> IORef [String] -> IORef Int -> String -> FilePath -> [String] -> [String]
  -> [String] -> TemplateNode -> IO String
compileOnBlock blockRef ctxAccum ctxRef modName src childMods alienNames tinkleNames (OnBlock pattern bodyNodes) = do
  bodyExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName src childMods alienNames tinkleNames bodyNodes
  let cleanPatterns = map trim (splitOnChar '/' pattern)
  mapM_ (either (ioError . userError) pure . validatePattern) cleanPatterns
  let clause pat = wrapPatternForChild childMods pat ++ " -> Just (" ++ bodyExpr ++ "); "
  pure $ concatMap clause cleanPatterns
compileOnBlock _ _ _ _ _ _ _ _ _ = pure ""

-- ---------------------------------------------------------------------------
-- Internal: on-block compilation (contextual, returns Just ([String], IO ()))
-- ---------------------------------------------------------------------------

compileOnBlockCtx
  :: IORef Int -> IORef [String] -> IORef Int -> String -> FilePath -> [String] -> [String]
  -> [String] -> TemplateNode -> IO String
compileOnBlockCtx blockRef ctxAccum ctxRef modName src childMods alienNames tinkleNames (OnBlock pattern bodyNodes) = do
  bodyExpr <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef modName src childMods alienNames tinkleNames bodyNodes
  let cleanPatterns = map trim (splitOnChar '/' pattern)
      ctxIds = extractContextualAlienIds bodyNodes
  mapM_ (either (ioError . userError) pure . validatePattern) cleanPatterns
  let clause pat = wrapPatternForChild childMods pat ++ " -> Just (" ++ show ctxIds ++ ", " ++ bodyExpr ++ "); "
  pure $ concatMap clause cleanPatterns
compileOnBlockCtx _ _ _ _ _ _ _ _ _ = pure ""
