-- | Harpe.Compiler.ViewGen — view-pass code generation.
--
-- Compiles @[TemplateNode]@ lists into @Html ()@ expressions for the
-- generated @view@ method of a Component instance.
module Harpe.Compiler.ViewGen
  ( compileNodesWithDeclsView
  , compileChunk
  , compileNode
  , compileOnBlock
  ) where

import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import Data.IORef (IORef)
import Harpe.AST
import Harpe.CodeGen.Names
import Harpe.Compiler.Common (includeInfoFromPath)
import Harpe.Compiler.NodeUtils (nextBlockId, splitOnChar)
import Harpe.Compiler.PatternValidator (validatePattern, wrapPatternForChild, stripDollarPrefix)
import Harpe.Parser.Combinators (trim)
import Harpe.Templates
  ( errCrudeInvalidContext, errInternalAlienNode
  , errImplyTransformOutsideCrude
  )

-- ---------------------------------------------------------------------------
-- Public: entry points
-- ---------------------------------------------------------------------------

-- | Compile a sequence of nodes, threading @HaskellDecl@ nodes as @let … in@
-- wrappers around the rest of the expression.
compileNodesWithDeclsView
  :: IORef Int -> String -> FilePath -> [String] -> [String]
  -> [TemplateNode] -> IO String
compileNodesWithDeclsView blockRef modName srcDir childMods alienNames nodes = do
  compileDecls nodes
  where
    compileDecls ns = case break isDecl ns of
      (nonDecls, []) ->
        compileSimpleNodes blockRef modName srcDir childMods alienNames nonDecls
      (nonDecls, HaskellDecl decl : rest) -> do
        prefix <- if null nonDecls
                    then pure ""
                    else (++ " >>\n        ") <$>
                           compileSimpleNodes blockRef modName srcDir childMods alienNames nonDecls
        restExpr <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames rest
        let cleanDecl = if "let " `isPrefixOf` decl then decl else "let " ++ decl
        pure $ prefix ++ "(" ++ cleanDecl ++ " in (" ++ restExpr ++ "))"
      (nonDecls, LetDecl bindings : rest) -> do
        prefix <- if null nonDecls
                    then pure ""
                    else (++ " >>\n        ") <$>
                           compileSimpleNodes blockRef modName srcDir childMods alienNames nonDecls
        compiledBindings <- mapM (\(bName, bArgs, bDef) -> do
          defStr <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames bDef
          let defE = if null defStr || defStr == "return ()" then "(return () :: Html ())" else "((" ++ defStr ++ ") :: Html ())"
          let argsStr = if null bArgs then "" else " " ++ unwords bArgs
          pure $ bName ++ argsStr ++ " = " ++ defE) bindings
        restExpr <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames rest
        let letStr = "let { " ++ intercalate "; " compiledBindings ++ " } in (" ++ restExpr ++ ")"
        pure $ prefix ++ "(" ++ letStr ++ ")"
      _ -> error "Should not happen"

    isDecl (HaskellDecl _) = True
    isDecl (LetDecl _)     = True
    isDecl _               = False



-- | Compile a ChunkBlock into a @let@-binding string: @name arg1 arg2 = body@.
compileChunk
  :: IORef Int -> String -> FilePath -> [String] -> [String]
  -> TemplateNode -> IO String
compileChunk blockRef modName srcDir childMods alienNames (ChunkBlock name args body) = do
  case mapM_ validateChunkNode body of
    Left err -> ioError (userError $ "harpe E002 \8212 invalid node inside chunk '" ++ name ++ "': " ++ err ++ "\n  Chunks are restricted to pure HTML and variables only.")
    Right () -> do
      compiledBody <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames body
      pure $ name ++ " " ++ unwords args ++ " = " ++ compiledBody
compileChunk _ _ _ _ _ _ = ioError (userError errNotAChunkBlock)
  where errNotAChunkBlock = "harpe internal: compileChunk called on a non-ChunkBlock node"

validateChunkNode :: TemplateNode -> Either String ()
validateChunkNode (EventBinding n _) = Left $ "Event bindings (" ++ n ++ ") are not allowed."
validateChunkNode (OnBlock _ _) = Left "'on' blocks are not allowed."
validateChunkNode (DefaultBlock _ _ _) = Left "'default' blocks are not allowed."
validateChunkNode (CrudeBlock _ _ _) = Left "'crude' blocks are not allowed."
validateChunkNode (AlienBlock _ _ _ _) = Left "'alien' blocks are not allowed."
validateChunkNode (AddAlien _ _ _) = Left "'add alien' is not allowed."
validateChunkNode (PropagateAlien _) = Left "'propagate alien' is not allowed."
validateChunkNode (PropagatedAlienBlock _ _ _ _) = Left "'propagate alien' is not allowed."
validateChunkNode (ImplyAlien _ _) = Left "'imply alien' is not allowed."
validateChunkNode (ImplyCrude _ _) = Left "'imply crude' is not allowed."
validateChunkNode (ClinchTemplate _ _) = Left "'clinch' components are not allowed."
validateChunkNode (LetBlock decls body) = mapM_ (\(_,_,def) -> mapM_ validateChunkNode def) decls >> mapM_ validateChunkNode body
validateChunkNode (LetDecl decls) = mapM_ (\(_,_,def) -> mapM_ validateChunkNode def) decls
validateChunkNode (RootBlock body) = mapM_ validateChunkNode body
validateChunkNode (ChunkBlock _ _ body) = mapM_ validateChunkNode body
validateChunkNode _ = Right ()

-- ---------------------------------------------------------------------------
-- Internal: node-level compilation
-- ---------------------------------------------------------------------------

compileSimpleNodes
  :: IORef Int -> String -> FilePath -> [String] -> [String]
  -> [TemplateNode] -> IO String
compileSimpleNodes _ _ _ _ _ [] = pure "return ()"
compileSimpleNodes blockRef modName srcDir childMods alienNames ns = do
  compiled <- mapM (compileNode blockRef modName srcDir childMods alienNames) ns
  pure (intercalate " >>\n        " (filter (not . null) compiled))

compileNode
  :: IORef Int -> String -> FilePath -> [String] -> [String]
  -> TemplateNode -> IO String

-- Raw HTML passthrough
compileNode _ _ _ _ _ (HtmlChunk s) =
  pure $ "writeHtml " ++ show s

-- Inline Haskell expression (or chunk invocation prefixed with "imply ")
compileNode _ _ _ _ _ (HaskellExpr expr)
  | ("imply " `isPrefixOf` expr) = pure expr
  | otherwise                     = pure $ "toHtml (" ++ expr ++ ")"

-- Event binding: alien variant dispatches MsgAlien
compileNode _ _ _ _ _ (EventBinding evtName ("alien":funcName:funcArgs)) =
  let attr = map toLower evtName
      ctor  = nmMsgAlien ++ " " ++ show funcName ++ " [" ++ intercalate "," funcArgs ++ "]"
  in pure $ "writeHtml (\" " ++ attr ++ "=\\\"\" <> dispatchJS (" ++ rtMsgWrappers ++ ") "
           ++ show ctor ++ " [] <> \"\\\"\")"

-- Event binding: parent message dispatch
compileNode _ _ _ _ _ (EventBinding evtName args) =
  let attr = map toLower evtName
      (ctor, inputs) = extractCtorAndInputs args
  in pure $ "writeHtml (\" " ++ attr ++ "=\\\"\" <> dispatchJS (" ++ rtMsgWrappers
           ++ " ++ [" ++ show nmMsgParent ++ "]) " ++ show ctor ++ " " ++ show inputs ++ " <> \"\\\"\")"

-- Clinch: delegate to child component view
compileNode _ _ _ _ _ (ClinchTemplate relPath mbHandler) =
  let info = includeInfoFromPath relPath mbHandler
      modN  = includeModuleName info
      fldN  = includeFieldName info
  in pure $
       "(Harpe.Core.view (" ++ rtMsgWrappers ++ " ++ [" ++ show (mkMsgCtor modN) ++ "]) "
       ++ "(" ++ fldN ++ "Model " ++ rtComposedModel ++ ") "
       ++ "(case " ++ rtActiveEvent ++ " of { Just (" ++ mkEventCtor modN ++ " e) -> Just e; _ -> Nothing }))"

-- DefaultBlock: render default content with optional on-pattern branches
compileNode blockRef modName srcDir childMods alienNames (DefaultBlock defaultPatterns defNodes onBlocks) = do
  blockId  <- nextBlockId blockRef modName
  defExpr  <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames defNodes
  onExprs  <- mapM (compileOnBlock blockRef modName srcDir childMods alienNames) onBlocks
  let defPats = concatMap
        (\pat -> nmEventParent ++ " (" ++ stripDollarPrefix pat ++ ") -> Just ("
                 ++ defExpr ++ ", \"default\"); ")
        defaultPatterns
  pure $ "default_view " ++ show blockId ++ " (" ++ defExpr ++ ") "
       ++ "( \\ mb -> on_view ( \\ event -> case event of { "
       ++ concat onExprs ++ defPats ++ "_ -> Nothing }) mb) " ++ rtActiveEvent

-- Crude / imply nodes have no view output
compileNode _ _ _ _ _ (CrudeBlock Nothing [] _) = pure "return ()"
compileNode _ _ _ _ _ (CrudeBlock mbName _ _) =
  ioError (userError (errCrudeInvalidContext (maybe "anonymous" id mbName)))
compileNode _ _ _ _ _ (ImplyCrude _ _) = pure "return ()"

-- Alien nodes should never appear as view body nodes
compileNode _ _ _ _ _ (AlienBlock _ _ _ _)           = ioError (userError (errInternalAlienNode "AlienBlock"))
compileNode _ _ _ _ _ (AddAlien _ _ _)               = ioError (userError (errInternalAlienNode "AddAlien"))
compileNode _ _ _ _ _ (PropagateAlien _)           = ioError (userError (errInternalAlienNode "PropagateAlien"))
compileNode _ _ _ _ _ (PropagatedAlienBlock _ _ _ _) = ioError (userError (errInternalAlienNode "PropagatedAlienBlock"))
compileNode _ _ _ _ _ (ImplyAlien name args)       = ioError (userError (errImplyTransformOutsideCrude name args))

-- These emit nothing in the view pass
compileNode _ _ _ _ _ WithoutMVU                       = pure "return ()"

-- These should never reach compileNode (partitioned / threaded before this point)
compileNode _ _ _ _ _ (OnBlock _ _)      = pure "return ()"
compileNode _ _ _ _ _ (HaskellDecl _)   = pure "return ()"
compileNode _ _ _ _ _ (LetDecl _)       = pure "return ()"
compileNode _ _ _ _ _ (ChunkBlock _ _ _) = pure "return ()"
compileNode _ _ _ _ _ (RootBlock _)     = pure "return ()"
compileNode blockRef modName srcDir childMods alienNames (LetBlock bindings bodyNodes) = do
  compiledBindings <- mapM (\(bName, bArgs, bDef) -> do
    defExpr <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames bDef
    let defE = if null defExpr || defExpr == "return ()" then "(return () :: Html ())" else "((" ++ defExpr ++ ") :: Html ())"
    let argsStr = if null bArgs then "" else " " ++ unwords bArgs
    pure $ bName ++ argsStr ++ " = " ++ defE) bindings
  bodyExpr <- compileNodesWithDeclsView blockRef modName srcDir childMods alienNames bodyNodes
  let bodyE = if null bodyExpr || bodyExpr == "return ()" then "(return () :: Html ())" else "((" ++ bodyExpr ++ ") :: Html ())"
  pure $ "let { " ++ intercalate "; " compiledBindings ++ " } in " ++ bodyE

-- ---------------------------------------------------------------------------
-- Internal: on-block compilation
-- ---------------------------------------------------------------------------

compileOnBlock
  :: IORef Int -> String -> FilePath -> [String] -> [String]
  -> TemplateNode -> IO String
compileOnBlock blockRef modName src childMods alienNames (OnBlock pattern bodyNodes) = do
  bodyExpr <- compileNodesWithDeclsView blockRef modName src childMods alienNames bodyNodes
  let cleanPatterns = map trim (splitOnChar '/' pattern)
  mapM_ (either (ioError . userError) pure . validatePattern) cleanPatterns
  let clause pat =
        wrapPatternForChild childMods pat ++ " -> Just (" ++ bodyExpr ++ ", \"on\"); "
  pure $ concatMap clause cleanPatterns
compileOnBlock _ _ _ _ _ _ = pure ""

-- ---------------------------------------------------------------------------
-- Internal: helper
-- ---------------------------------------------------------------------------

extractCtorAndInputs :: [String] -> (String, [String])
extractCtorAndInputs [] = ("Clicked", [])
extractCtorAndInputs ("emit":c:ins) = parseCtorParen c ins
extractCtorAndInputs (c:ins)        = parseCtorParen c ins

parseCtorParen :: String -> [String] -> (String, [String])
parseCtorParen c ins =
  let joined = unwords (c : ins)
  in case joined of
       ('(':xs) | not (null xs) && last xs == ')' -> (init xs, [])
       _                                           -> (c, ins)
