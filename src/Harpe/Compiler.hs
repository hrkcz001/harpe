-- | Harpe.Compiler — top-level template compilation orchestrator.
--
-- This module owns @compileTemplate@, which stitches together all the
-- sub-passes: preprocessing, AST partitioning, alien collection, view/effects
-- code generation, and final module assembly.
--
-- Every sub-pass lives in a focused sub-module:
--   Harpe.Compiler.NodeUtils      — AST partitioning, model-type helpers
--   Harpe.Compiler.PatternValidator — on-block pattern validation
--   Harpe.Compiler.ViewGen        — view-pass code generation
--   Harpe.Compiler.EffectsGen     — effects-pass code generation
--   Harpe.Compiler.Alien          — alien / FFI codegen
--   Harpe.Compiler.Common         — shared utilities (file IO, word replacement)
module Harpe.Compiler where

import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, partition)
import Data.IORef (newIORef)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import System.Directory (doesFileExist, doesDirectoryExist)

import System.Environment (getExecutablePath)
import Harpe.AST
import Harpe.Parser
import Harpe.CodeGen.Names
import Harpe.Compiler.Common (readUtf8File, replaceWordBoundaries, replaceSubstring)
import Harpe.Compiler.NodeUtils (partitionNodes, PartitionedNodes(..), collectIncludes, getModelType, isStateOrModelDecl, compileCrudeDecl)
import Harpe.Compiler.ViewGen (compileNodesWithDeclsView, compileChunk)
import Harpe.Compiler.EffectsGen (compileNodesWithDeclsEffects, ctxAccumFetch)
import Harpe.Compiler.Alien (compileAlien, nubByAlienName)
import Harpe.Templates
  ( errTemplatesDirNotFound, errBoilerplateNotFound
  , errAlienFileNotFound, errAlienNotFound, errAlienFileParseError
  , errInternalAlienNode
  , moduleFixedPragmas, harpeCoreMvuImport, harpeCoreMinImport, ffiPreamble
  )

-- ---------------------------------------------------------------------------
-- Alien collection
-- ---------------------------------------------------------------------------

-- | Collect all alien nodes reachable from @nodes@, following @add@ directives.
collectAliens :: FilePath -> [TemplateNode] -> IO [TemplateNode]
collectAliens currentDir nodes = do
  let pn       = partitionNodes nodes
      localAls  = pLocalAliens pn
      addAls    = pAddAliens   pn
  nested <- mapM (resolveAddAlien currentDir Nothing) addAls
  pure (localAls ++ concat nested)

-- | Like 'collectAliens' but filtered to a specific target name.
collectAliensFiltered :: FilePath -> String -> [TemplateNode] -> IO [TemplateNode]
collectAliensFiltered currentDir target nodes = do
  let pn        = partitionNodes nodes
      localAls   = pLocalAliens pn
      addAls     = pAddAliens   pn
      matchedLocal = filter (matchesTarget target) localAls
  nested <- mapM (resolveAddAlien currentDir (Just target)) addAls
  pure (filter (matchesTarget target) (matchedLocal ++ concat nested))

-- | Resolve a single @AddAlien@ node: load the referenced file, parse it,
-- and recursively collect aliens matching the (optional) target.
resolveAddAlien :: FilePath -> Maybe String -> TemplateNode -> IO [TemplateNode]
resolveAddAlien currentDir mbTarget (AddAlien className instName path) = do
  let fullPath = currentDir </> path
  exists <- doesFileExist fullPath
  if not exists
    then ioError (userError (errAlienFileNotFound fullPath))
    else do
      if maybe False (/= instName) mbTarget
        then pure []
        else do
          content <- readUtf8File fullPath
          case parseTemplate content of
            Left err  -> ioError (userError (errAlienFileParseError fullPath err))
            Right ast -> do
              result <- collectAliensFiltered (takeDirectory fullPath) className (templateNodes ast)
              let renamed = map renameAlien result
                  renameAlien (AlienBlock n a b t) | n == className = AlienBlock instName a b t
                  renameAlien (PropagatedAlienBlock n a b t) | n == className = PropagatedAlienBlock instName a b t
                  renameAlien other = other
              if null renamed
                then ioError (userError (errAlienNotFound className fullPath))
                else pure renamed
resolveAddAlien _ _ _ = ioError (userError (errInternalAlienNode "non-AddAlien in addAliens list"))

-- | True if a node belongs to the named target (or is a catch-all kind).
matchesTarget :: String -> TemplateNode -> Bool
matchesTarget target node = case node of
  AlienBlock name _ _ _         -> target == name
  _                           -> False

-- | Collect the import lines that result from @add alien … from File.harpe@ directives.
collectAddImports :: FilePath -> [TemplateNode] -> IO ([String], [String])
collectAddImports currentDir nodes = do
  let addAls = pAddAliens (partitionNodes nodes)
  results <- mapM resolveImport addAls
  pure (concatMap fst results, concatMap snd results)
  where
    resolveImport (AddAlien className instName path) = do
      let fullPath   = currentDir </> path
          modName    = takeBaseName path
      exists <- doesFileExist fullPath
      if not exists
        then ioError (userError (errAlienFileNotFound fullPath))
        else do
          allNested <- collectAliensFiltered (takeDirectory fullPath) className [AddAlien className className path]
          let names = [ n | AlienBlock n _ _ _        <- allNested ]
              hasTinkle n = any (\node -> case node of AlienBlock n2 _ _ (Just _) -> n2 == n; _ -> False) allNested
              hasInstTinkle = hasTinkle className
          if null names
            then ioError (userError (errAlienNotFound className fullPath))
            else do
              let imports = ["import qualified " ++ modName]
                  aliases = if hasInstTinkle
                              then [ instName ++ " = " ++ modName ++ "." ++ className
                                   , instName ++ nmTinkleSuffix ++ " = " ++ modName ++ "." ++ className ++ nmTinkleSuffix
                                   ]
                              else [ instName ++ " = " ++ modName ++ "." ++ className ]
              pure (imports, aliases)
    resolveImport _ = ioError (userError (errInternalAlienNode "non-AddAlien in addAliens list"))

-- ---------------------------------------------------------------------------
-- Templates-directory discovery
-- ---------------------------------------------------------------------------

-- | Locate the @templates/@ directory relative to the harpe executable.
-- Tries several sibling paths (cabal-run, installed) and falls back to
-- @./templates@ with a warning.
findTemplatesDir :: IO FilePath
findTemplatesDir = do
  exePath <- getExecutablePath
  let exeDir    = takeDirectory exePath
      candidates =
        [ exeDir </> "templates"
        , exeDir </> ".." </> "templates"
        , exeDir </> ".." </> ".." </> "templates"
        , exeDir </> ".." </> ".." </> ".." </> "templates"
        ]
  found <- findFirst candidates
  case found of
    Just d  -> pure d
    Nothing -> do
      putStrLn errTemplatesDirNotFound
      pure "templates"
  where
    findFirst []     = pure Nothing
    findFirst (d:ds) = do
      exists <- doesDirectoryExist d
      if exists then pure (Just d) else findFirst ds

-- ---------------------------------------------------------------------------
-- compileTemplate — the main entry point
-- ---------------------------------------------------------------------------

-- | Compile a parsed 'TemplateAST' into a Haskell source string.
compileTemplate :: FilePath -> String -> TemplateAST -> IO String
compileTemplate sourcePath moduleName ast = do
  let imps  = templateImports ast
      decls = templateDeclarations ast
      nodes = templateNodes ast
      srcDir = takeDirectory sourcePath

  -- ── Model type ──────────────────────────────────────────────────────────
  let modelType = getModelType decls

  -- ── Declaration classification ──────────────────────────────────────────
  let isMsgDecl s       = case words s of ("data":"Msg":_)    -> True; ("newtype":"Msg":_)    -> True; ("type":"Msg":_)    -> True; _ -> False
      isEventDecl s     = case words s of ("data":"Event":_)  -> True; ("newtype":"Event":_)  -> True; ("type":"Event":_)  -> True; _ -> False
      isUpdateDecl s    = case words s of ("update":_)        -> True; _ -> False
      isInitModelDecl s = case words s of ("initModel":_)     -> True; _ -> False

  let parentMsgLines   = filter isMsgDecl       decls
      parentEventLines = filter isEventDecl     decls
      parentUpdateLines= filter isUpdateDecl    decls
      parentInitLines  = filter isInitModelDecl decls

      customDeclsAll = filter (\d -> not (isStateOrModelDecl d || isMsgDecl d || isEventDecl d || isUpdateDecl d || isInitModelDecl d)) decls
      (pragmas, customDecls) = partition (\d -> "{-#" `isPrefixOf` dropWhile isSpace d) customDeclsAll

  -- ── Parent-layer declarations ────────────────────────────────────────────
  let parentMsgDecl =
        if null parentMsgLines
          then "data " ++ nmParentMsg ++ " = " ++ nmParentMsgDummy ++ " deriving (Show, Read)"
          else unlines (map renameMsgDecl parentMsgLines)

      parentEventDecl =
        if null parentEventLines
          then "data " ++ nmParentEvent ++ " = " ++ nmParentEventDummy ++ " deriving (Show, Read)"
          else unlines (map renameEventDecl parentEventLines)

      parentUpdateDecl =
        if null parentUpdateLines
          then nmParentUpdate ++ " :: " ++ nmParentMsg ++ " -> " ++ nmParentModel
               ++ " -> (" ++ nmParentModel ++ ", [" ++ nmParentEvent ++ "])\n"
               ++ nmParentUpdate ++ " _ m = (m, [])"
          else unlines (map renameUpdateLine parentUpdateLines)

      hasInitModel    = not (null parentInitLines)
      parentInitDecl  =
        if hasInitModel
          then unlines (map renameInitModelDecl parentInitLines)
          else nmInitParentModel ++ " :: " ++ nmParentModel ++ "\n"
               ++ nmInitParentModel ++ " = " ++ (if modelType == "()" then "()" else "error \"initParentModel not defined\"")

  -- ── AST partitioning ────────────────────────────────────────────────────
  let pn           = partitionNodes nodes
      includeInfos = collectIncludes nodes
      childModules = map includeModuleName includeInfos
      chunks       = pChunks     pn
      mbRoot       = pRoot       pn
      crudeDecls   = pCrudeDecls pn
      localAliens  = pLocalAliens pn
      mainBodyNodes = case mbRoot of { Just rNodes -> rNodes; Nothing -> pOthers pn }
      hasWithout   = WithoutMVU `elem` nodes

  -- ── Alien collection ────────────────────────────────────────────────────
  allAliens    <- collectAliens srcDir nodes
  (addedImports, addedAliases) <- collectAddImports srcDir nodes
  let uniqueAliens = nubByAlienName allAliens
      alienNames   = [ n | AlienBlock n _ _ _     <- uniqueAliens ]
      compiledAliens = map compileAlien localAliens
      uniqueProps  = [ name | PropagateAlien name <- pProps pn ]
      -- Extract names of aliens that have a tinkle cleanup
      alienTinkleNames = [ n | AlienBlock n _ _ (Just _) <- uniqueAliens ]

  -- ── Crude declarations ──────────────────────────────────────────────────
  compiledCrudes <- case sequence (map (compileCrudeDecl alienNames) crudeDecls) of
    Left err -> ioError (userError err)
    Right cs -> pure cs

  -- ── View / effects generation ───────────────────────────────────────────
  blockRef <- newIORef 0
  ctxAccum <- newIORef []  -- accumulates top-level IORef declarations for contextual aliens
  ctxRef   <- newIORef 0   -- counter for ctx IORef names
  mainBodyView    <- compileNodesWithDeclsView    blockRef moduleName srcDir childModules alienNames mainBodyNodes
  mainBodyEffects <- compileNodesWithDeclsEffects blockRef ctxAccum ctxRef moduleName srcDir childModules alienNames alienTinkleNames mainBodyNodes
  ctxDecls <- ctxAccumFetch ctxAccum
  compiledChunksView <- mapM (compileChunk blockRef moduleName srcDir childModules alienNames) chunks

  -- ── View / effects body strings ─────────────────────────────────────────
  let modelBind    = rtModel ++ " = " ++ (if null includeInfos then rtComposedModel else rtParentModel ++ " " ++ rtComposedModel)
      bindingsView = intercalate " ; " (modelBind : compiledChunksView)
      viewBody     = "let { " ++ bindingsView ++ " } in " ++ mainBodyView
      effectsBody  = mainBodyEffects

  -- ── Wrapper type names ───────────────────────────────────────────────────
  let wrapperMsgName   = mkWrapperMsgName   moduleName
      wrapperEventName = mkWrapperEventName moduleName

  -- ── Export list ──────────────────────────────────────────────────────────
  let propTinkleNames = [ name ++ nmTinkleSuffix | name <- uniqueProps, name `elem` alienTinkleNames ]
      exportList
        | hasWithout = intercalate ", " (uniqueProps ++ propTinkleNames)
        | otherwise  = intercalate ", " $
            [ nmModel, nmInitModel
            , moduleName ++ "." ++ nmUpdate
            , nmDecodeMsg, nmStepRenderWire, nmRunWireMessages
            , nmParentModel, nmInitParentModel, nmParentUpdate
            , nmParentMsg ++ "(..)", nmParentEvent ++ "(..)"
            , wrapperMsgName ++ "(..)", wrapperEventName ++ "(..)"
            , nmDomEnv
            ] ++ uniqueProps

  -- ── Type / value declarations ────────────────────────────────────────────
  let modelDecl
        | null includeInfos = "type " ++ nmModel ++ " = " ++ nmParentModel
        | otherwise =
            "data " ++ nmModel ++ " = " ++ nmModel ++ " { " ++
            intercalate ", " (
              (rtParentModel ++ " :: " ++ nmParentModel) :
              [ includeFieldName info ++ "Model :: " ++ includeModuleName info ++ "." ++ nmModel
              | info <- includeInfos ]) ++
            " } deriving (Show)"

      initDecl
        | null includeInfos =
            nmInitModel ++ " :: " ++ nmModel ++ "\n" ++ nmInitModel ++ " = " ++ nmInitParentModel
        | otherwise =
            nmInitModel ++ " :: " ++ nmModel ++ "\n" ++
            nmInitModel ++ " = " ++ nmModel ++ " { " ++ rtParentModel ++ " = " ++ nmInitParentModel ++ ", " ++
            intercalate ", "
              [ includeFieldName info ++ "Model = " ++ includeModuleName info ++ "." ++ nmInitModel
              | info <- includeInfos ] ++ " }"

      msgDecl
        | null includeInfos =
            "data " ++ wrapperMsgName ++ " = " ++ nmMsgParent ++ " " ++ nmParentMsg
            ++ " | " ++ nmMsgAlien ++ " T.Text [T.Text] deriving (Show, Read)"
        | otherwise =
            "data " ++ wrapperMsgName ++ " = " ++ nmMsgParent ++ " " ++ nmParentMsg
            ++ " | " ++ nmMsgAlien ++ " T.Text [T.Text] | " ++
            intercalate " | "
              [ "Msg" ++ includeModuleName info ++ " " ++ includeModuleName info ++ "." ++ includeModuleName info ++ "Msg"
              | info <- includeInfos ]
            ++ " deriving (Show, Read)"

      eventDecl
        | null includeInfos =
            "data " ++ wrapperEventName ++ " = " ++ nmEventParent ++ " " ++ nmParentEvent
            ++ " | " ++ nmEventAlien ++ " T.Text [T.Text] deriving (Show, Read)"
        | otherwise =
            "data " ++ wrapperEventName ++ " = " ++ nmEventParent ++ " " ++ nmParentEvent
            ++ " | " ++ nmEventAlien ++ " T.Text [T.Text] | " ++
            intercalate " | "
              [ "Event" ++ includeModuleName info ++ " " ++ includeModuleName info ++ "." ++ includeModuleName info ++ "Event"
              | info <- includeInfos ]
            ++ " deriving (Show, Read)"

      msgAliasDecl   = "type Msg = " ++ nmParentMsg
      eventAliasDecl = "type Event = " ++ nmParentEvent

      updateDecl = unlines $
        ("update :: " ++ wrapperMsgName ++ " -> " ++ nmModel ++ " -> (" ++ nmModel ++ ", [" ++ wrapperEventName ++ "])") :
        parentClause :
        alienClause  :
        childClauses
        where
          parentClause =
            "update (" ++ nmMsgParent ++ " pMsg) model = " ++
            if null includeInfos
              then "let (nextM, pEvts) = " ++ nmParentUpdate ++ " pMsg model in (nextM, map " ++ nmEventParent ++ " pEvts)"
              else "let (nextM, pEvts) = " ++ nmParentUpdate ++ " pMsg (" ++ rtParentModel ++ " model) in (model { " ++ rtParentModel ++ " = nextM }, map " ++ nmEventParent ++ " pEvts)"
          alienClause =
            "update (" ++ nmMsgAlien ++ " func args) model = (model, [" ++ nmEventAlien ++ " func args])"
          childClauses =
            [ "update (Msg" ++ includeModuleName info ++ " cMsg) model =\n" ++
              "  let (nextChildModel, cEvents) = " ++ includeModuleName info ++ ".update cMsg (" ++ includeFieldName info ++ "Model model)\n" ++
              "      cEventsAlien = [ (func, args) | " ++ includeModuleName info ++ "." ++ nmEventAlien ++ " func args <- cEvents ]\n" ++
              "      cEventsOther = [ e | e <- cEvents, case e of { " ++ includeModuleName info ++ "." ++ nmEventAlien ++ " _ _ -> False; _ -> True } ]\n" ++
              "      parentAlienMsgs = [ " ++ nmMsgAlien ++ " func args | (func, args) <- cEventsAlien ]\n" ++
              "      parentMsgs = parentAlienMsgs ++ " ++
              (case includeHandlerName info of
                Just h  -> "concatMap (\\e -> case " ++ h ++ " e of { Just msg -> [" ++ nmMsgParent ++ " msg]; Nothing -> [] }) cEventsOther"
                Nothing -> "[]") ++ "\n" ++
              "      remainingEvents = map Event" ++ includeModuleName info ++ " cEventsOther\n" ++
              "      (nextModel, parentEvents) = foldl (\\(m, evts) pMsg -> let (m', evts') = " ++ moduleName ++ ".update pMsg m in (m', evts ++ evts')) (model { " ++ includeFieldName info ++ "Model = nextChildModel }, []) parentMsgs\n" ++
              "  in (nextModel, parentEvents ++ remainingEvents)"
            | info <- includeInfos ]

      includeImports = concat
        [ [ "import qualified " ++ includeModuleName info
          , "import " ++ includeModuleName info ++ " (" ++ includeModuleName info ++ "Event)"
          ]
        | info <- includeInfos ]

      ctxImport
        | null ctxDecls = []
        | otherwise     =
            [ "import System.IO.Unsafe (unsafePerformIO)"
            , "import Data.IORef (IORef, newIORef)"
            ]

      ffiImports
        | null uniqueAliens = []
        | otherwise         = ffiPreamble

      compiledAliensCases = concat
        [ "      (" ++ show name ++ ", [" ++
          intercalate "," (take (length args) (map (\i -> "a" ++ show i) [1::Int ..])) ++
          "]) -> " ++ name ++ " " ++
          unwords (take (length args) (map (\i -> "a" ++ show i) [1::Int ..])) ++ "\n"
        | AlienBlock name args _ _ <- uniqueAliens ]

      viewDecl =
        "  view " ++ rtMsgWrappers ++ " " ++ rtComposedModel ++ " " ++ rtActiveEvent ++ " = case " ++ rtActiveEvent ++ " of\n" ++
        "    Just (" ++ nmEventAlien ++ " _ _) -> Harpe.Core.view " ++ rtMsgWrappers ++ " " ++ rtComposedModel ++ " Nothing\n" ++
        "    _ -> " ++ viewBody

      effectsDecl =
        "  effects " ++ rtMsgWrappers ++ " " ++ rtComposedModel ++ " " ++ rtActiveEvent ++ " = case " ++ rtActiveEvent ++ " of\n" ++
        "    Just (" ++ nmEventAlien ++ " func args) -> case (func, args) of\n" ++
        compiledAliensCases ++
        "      _ -> return ()\n" ++
        "    _ -> " ++ effectsBody

  -- ── Final module string assembly ─────────────────────────────────────────
  let code
        | hasWithout = unlines $
            pragmas ++ moduleFixedPragmas ++
            [ "module " ++ moduleName ++ " (" ++ exportList ++ ") where"
            , ""
            , harpeCoreMinImport
            ] ++ addedImports ++ imps ++ ffiImports ++
            [ ""
            , "-- Declarations from template:"
            ] ++ addedAliases ++ customDecls ++ compiledAliens
        | otherwise  = unlines $
            pragmas ++ moduleFixedPragmas ++
            [ "module " ++ moduleName ++ " (" ++ exportList ++ ") where"
            , ""
            , harpeCoreMinImport
            , harpeCoreMvuImport
            ] ++ ctxImport ++ includeImports ++ addedImports ++ imps ++ ffiImports ++
            [ ""
            , msgAliasDecl
            , eventAliasDecl
            , ""
            , "-- Declarations from template:"
            ] ++ addedAliases ++ customDecls ++ compiledCrudes ++ compiledAliens ++ ctxDecls ++
            [ ""
            , "type " ++ nmParentModel ++ " = " ++ modelType
            , parentInitDecl
            , parentMsgDecl
            , parentEventDecl
            , parentUpdateDecl
            , ""
            , modelDecl
            , initDecl
            , msgDecl
            , eventDecl
            , updateDecl
            , ""
            ]

  extraCode <- if hasWithout
    then pure ""
    else do
      templatesDir <- findTemplatesDir
      let boilerplatePath = templatesDir </> "module_boilerplate.hs"
      exists <- doesFileExist boilerplatePath
      if not exists
        then ioError (userError $ errBoilerplateNotFound boilerplatePath)
        else do
          raw <- readUtf8File boilerplatePath
          let filled = replaceSubstring "{{MODULE_NAME}}"   moduleName
                     $ replaceSubstring "{{WRAPPER_MSG}}"   wrapperMsgName
                     $ replaceSubstring "{{WRAPPER_EVENT}}" wrapperEventName
                       raw
              -- Strip the comment header lines at the top of the template
              stripped = unlines . dropWhile (\l -> take 2 l == "--" || null l) . lines $ filled
          pure $ "\n" ++ stripped ++ "\n" ++ viewDecl ++ "\n\n" ++ effectsDecl ++ "\n"

  pure (code ++ extraCode)

-- ---------------------------------------------------------------------------
-- Declaration-renaming helpers (private to this module)
-- ---------------------------------------------------------------------------

renameMsgDecl :: String -> String
renameMsgDecl s = case words s of
  ("data"   :"Msg":rest) -> "data "    ++ nmParentMsg ++ " " ++ unwords rest ++ autoDerive rest
  ("newtype":"Msg":rest) -> "newtype " ++ nmParentMsg ++ " " ++ unwords rest ++ autoDerive rest
  ("type"   :"Msg":rest) -> "type "    ++ nmParentMsg ++ " " ++ unwords rest
  _                      -> s
  where autoDerive ws = if "deriving" `elem` ws then "" else " deriving (Show, Read)"

renameEventDecl :: String -> String
renameEventDecl s = case words s of
  ("data"   :"Event":rest) -> "data "    ++ nmParentEvent ++ " " ++ unwords rest ++ autoDerive rest
  ("newtype":"Event":rest) -> "newtype " ++ nmParentEvent ++ " " ++ unwords rest ++ autoDerive rest
  ("type"   :"Event":rest) -> "type "    ++ nmParentEvent ++ " " ++ unwords rest
  _                        -> s
  where autoDerive ws = if "deriving" `elem` ws then "" else " deriving (Show, Read)"

renameUpdateLine :: String -> String
renameUpdateLine s = case words s of
  ("update":rest)
    | "::" `elem` rest -> nmParentUpdate ++ " " ++ replaceWordBoundaries (unwords rest)
    | otherwise        -> nmParentUpdate ++ " " ++ unwords rest
  _                    -> s

renameInitModelDecl :: String -> String
renameInitModelDecl s = case words s of
  ("initModel":rest)
    | "::" `elem` rest -> nmInitParentModel ++ " " ++ replaceWordBoundaries (unwords rest)
    | otherwise        -> nmInitParentModel ++ " " ++ unwords rest
  _                    -> s