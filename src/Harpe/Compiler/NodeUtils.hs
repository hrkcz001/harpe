-- | Harpe.Compiler.NodeUtils — AST partitioning and shared code-generation helpers.
module Harpe.Compiler.NodeUtils
  ( partitionNodes
  , PartitionedNodes(..)
  , collectIncludes
  , extractModelType
  , getModelType
  , isStateOrModelDecl
  , processCrudeLine
  , compileCrudeDecl
  , nextBlockId
  , splitOnChar
  , isBlockNode
  ) where



import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Harpe.AST
import Harpe.CodeGen.Names (mkBlockId)
import Harpe.Compiler.Common (replaceSpecialChars, replaceSubstring, includeInfoFromPath)
import Harpe.Templates (errAlienUnsafeCall)

-- ---------------------------------------------------------------------------
-- AST partitioning
-- ---------------------------------------------------------------------------

-- | All buckets produced by a single pass over a flat node list.
data PartitionedNodes = PartitionedNodes
  { pChunks     :: [TemplateNode]         -- ChunkBlock nodes
  , pRoot       :: Maybe [TemplateNode]   -- body of the RootBlock, if any
  , pCrudeDecls :: [TemplateNode]         -- named CrudeBlock declarations
  , pLocalAliens :: [TemplateNode]        -- AlienBlock / InformerBlock
  , pAddAliens  :: [TemplateNode]         -- AddAlien directives
  , pProps      :: [TemplateNode]         -- PropagateAlien directives
  , pOthers     :: [TemplateNode]         -- everything else (body nodes)
  }

-- | Partition a flat @[TemplateNode]@ list into the seven buckets used by
-- the compiler.  Single left-fold — O(n).
partitionNodes :: [TemplateNode] -> PartitionedNodes
partitionNodes nodes = go nodes ([], Nothing, [], [], [], [], [])
  where
    build (cs, r, crudes, aliens, adds, props, os) =
      PartitionedNodes (reverse cs) r (reverse crudes) (reverse aliens)
                       (reverse adds) (reverse props) (reverse os)

    go [] acc = build acc
    go (n:rest) (cs, r, crudes, aliens, adds, props, os) = case n of
      ChunkBlock name args body ->
        go rest (ChunkBlock name args body : cs, r, crudes, aliens, adds, props, os)
      RootBlock body ->
        go rest (cs, Just body, crudes, aliens, adds, props, os)
      CrudeBlock name args body ->
        go rest (cs, r, CrudeBlock name args body : crudes, aliens, adds, props, os)
      AlienBlock name args body mbTinkle ->
        go rest (cs, r, crudes, AlienBlock name args body mbTinkle : aliens, adds, props, os)
      AddAlien className instName path ->
        go rest (cs, r, crudes, aliens, AddAlien className instName path : adds, props, os)
      PropagatedAlienBlock name args body mbTinkle ->
        go rest (cs, r, crudes, AlienBlock name args body mbTinkle : aliens, adds, PropagateAlien name : props, os)
      PropagateAlien name ->
        go rest (cs, r, crudes, aliens, adds, PropagateAlien name : props, os)
      WithoutMVU ->
        go rest (cs, r, crudes, aliens, adds, props, os)
      other ->
        go rest (cs, r, crudes, aliens, adds, props, other : os)

-- ---------------------------------------------------------------------------
-- Include / clinch utilities
-- ---------------------------------------------------------------------------

-- | Collect all IncludeInfo records by walking the entire AST recursively.
collectIncludes :: [TemplateNode] -> [IncludeInfo]
collectIncludes = concatMap collectOne
  where
    collectOne (ClinchTemplate relPath mbHandler) = [includeInfoFromPath relPath mbHandler]
    collectOne (DefaultBlock _ defNodes onBlocks) = collectIncludes defNodes ++ collectIncludes onBlocks
    collectOne (OnBlock _ bodyNodes)              = collectIncludes bodyNodes
    collectOne (ChunkBlock _ _ bodyNodes)         = collectIncludes bodyNodes
    collectOne (RootBlock bodyNodes)              = collectIncludes bodyNodes
    collectOne _                                  = []

-- ---------------------------------------------------------------------------
-- Model-type extraction helpers
-- ---------------------------------------------------------------------------

-- | Extract the RHS of a @type Model = …@ or @type State = …@ declaration.
extractModelType :: String -> Maybe String
extractModelType s = case words s of
  ("type":"Model":"=":rest)  -> Just (unwords rest)
  ("type":"Model":rest)      -> stripLeadingEq rest
  ("type":"State":"=":rest)  -> Just (unwords rest)
  ("type":"State":rest)      -> stripLeadingEq rest
  _                          -> Nothing
  where
    stripLeadingEq ((w:r) : ws)
      | w == '='  = Just (unwords (drop 1 [w:r] ++ ws))
      | otherwise = Nothing
    stripLeadingEq _ = Nothing

-- | Return the model type alias, or @"()"@ if none is declared.
getModelType :: [String] -> String
getModelType decls = case [t | Just t <- map extractModelType decls] of
  (t:_) -> t
  []    -> "()"

-- | True for @type State …@ or @type Model …@ declarations.
isStateOrModelDecl :: String -> Bool
isStateOrModelDecl s = case words s of
  ("type":"State":_) -> True
  ("type":"Model":_) -> True
  _                  -> False

-- ---------------------------------------------------------------------------
-- Crude-block processing
-- ---------------------------------------------------------------------------

-- | Check every word in a crude body line for bare alien names (which would
-- be an unsafe call bypassing the @imply transform@ / @imply inform@ guards).
-- On success, strips the guard prefix and returns the plain call expression.
processCrudeLine :: [String] -> String -> Either String String
processCrudeLine alienNames line =
  let wds          = words (replaceSpecialChars line)
      indexedWords  = zip [0..] wds
      -- Accept both "imply alien <name>" and "imply contextual alien <name>"
      isGuarded i _ =
        (i >= 2 && wds !! (i-1) == "alien" && wds !! (i-2) == "imply")
        || (i >= 3 && wds !! (i-1) == "alien" && wds !! (i-2) == "contextual" && wds !! (i-3) == "imply")
      checkWord (i, w)
        | w `elem` alienNames = if isGuarded i w then Right () else Left (errAlienUnsafeCall w)
        | otherwise           = Right ()
  in case mapM_ checkWord indexedWords of
       Left err -> Left err
       Right () ->
         let strip currLine aln =
               if ("imply contextual alien " ++ aln) `isInfixOf` currLine
                 then "Harpe.Core.harpe_ctx_guard " ++ show aln ++ " (" ++ replaceSubstring ("imply contextual alien " ++ aln) aln currLine ++ ")"
                 else replaceSubstring ("imply alien " ++ aln) aln currLine
         in Right (foldl strip line alienNames)

-- | Compile a *named* CrudeBlock into a top-level Haskell @where@ declaration.
compileCrudeDecl :: [String] -> TemplateNode -> Either String String
compileCrudeDecl alienNames (CrudeBlock (Just name) args bodyLines) =
  case mapM (processCrudeLine alienNames) bodyLines of
    Left err -> Left err
    Right processedLines ->
      let body = unlines (map ("  " ++) processedLines)
      in Right $ name ++ " " ++ unwords args ++ " = do\n" ++ body
compileCrudeDecl _ _ = Right ""

-- ---------------------------------------------------------------------------
-- Block-ID counter
-- ---------------------------------------------------------------------------

-- | Read the current counter, increment it, and return a unique DOM block id.
nextBlockId :: IORef Int -> String -> IO String
nextBlockId ref modName = do
  val <- readIORef ref
  writeIORef ref (val + 1)
  pure (mkBlockId modName val)

-- ---------------------------------------------------------------------------
-- Shared utility
-- ---------------------------------------------------------------------------

-- | Split a list on a delimiter character (non-empty segments only).
splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = []
splitOnChar d xs =
  let (val, rest) = break (== d) xs
  in val : case rest of
             []     -> []
             (_:ys) -> splitOnChar d ys

-- ---------------------------------------------------------------------------
-- Scoping Utilities
-- ---------------------------------------------------------------------------

isBlockNode :: TemplateNode -> Bool
isBlockNode (DefaultBlock {}) = True
isBlockNode (OnBlock {}) = True
isBlockNode (CrudeBlock {}) = True
isBlockNode (ChunkBlock {}) = True
isBlockNode (RootBlock {}) = True
isBlockNode (AlienBlock {}) = True
isBlockNode (LetBlock {}) = True
isBlockNode _ = False


