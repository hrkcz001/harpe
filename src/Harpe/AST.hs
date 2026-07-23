module Harpe.AST where

data TemplateNode
  = HtmlChunk String
  | HaskellExpr String
  | EventBinding String [String]    -- e.g. EventBinding "onClick" ["emit", "Clicked"]
  | ClinchTemplate String (Maybe String)
  | DefaultBlock [String] [TemplateNode] [TemplateNode] -- Default trigger patterns, Default nodes, OnBlock nodes
  | OnBlock String [TemplateNode]   -- Pattern string, body nodes
  | HaskellDecl String              -- Local Haskell code like "let res = ..."
  | ChunkBlock String [String] [TemplateNode]
  | RootBlock [TemplateNode]
  | CrudeBlock (Maybe String) [String] [String]
  | ImplyCrude String [String]
  | AlienBlock String [String] [String] (Maybe [String])
  | AddAlien String String String
  | ImplyAlien String [String]
  | PropagateAlien String
  | PropagatedAlienBlock String [String] [String] (Maybe [String])
  | WithoutMVU
  | LetBlock [(String, [String], [TemplateNode])] [TemplateNode]
  | LetDecl [(String, [String], [TemplateNode])]
  deriving (Show, Eq)

data TemplateAST = TemplateAST
  { templateImports      :: [String]
  , templateDeclarations :: [String]
  , templateNodes        :: [TemplateNode]
  } deriving (Show, Eq)

data IncludeInfo = IncludeInfo
  { includeModuleName :: String
  , includeFieldName :: String
  , includeHandlerName :: Maybe String
  } deriving (Show, Eq)
