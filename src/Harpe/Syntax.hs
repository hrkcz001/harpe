-- | Harpe.Syntax — DSL grammar constants.
--
-- Every keyword and delimiter of the harpe template language lives here.
-- Parser.hs imports these constants instead of inlining raw string literals,
-- so that adding or renaming a keyword requires a change in exactly one place.
--
-- Design: this is a /leaf/ module — it must not import any other Harpe.* module.
module Harpe.Syntax where

-- ---------------------------------------------------------------------------
-- Delimiters
-- ---------------------------------------------------------------------------

-- | Block / inline opener: the @\/\/=@ prefix.
symOpen :: String
symOpen = "//="

-- | Block / inline closer: the @=\/\/@ suffix.
symClose :: String
symClose = "=//"

-- | Alternative closer accepted by @without mvu@: @==\/@ (note the extra @=@).
symAltClose :: String
symAltClose = "==//"

-- | Length of 'symOpen'.  Use @drop symOpenLen@ instead of the magic number 3.
symOpenLen :: Int
symOpenLen = length symOpen

-- ---------------------------------------------------------------------------
-- Block-opening keywords (push a frame onto the preprocessor / validator stack)
-- ---------------------------------------------------------------------------

kwRoot      :: String
kwRoot      = "root"

kwChunk     :: String
kwChunk     = "chunk"

kwCrude     :: String
kwCrude     = "crude"

kwDefault   :: String
kwDefault   = "default"

kwOn        :: String
kwOn        = "on"

kwAlien     :: String
kwAlien     = "alien"

kwPropagate :: String
kwPropagate = "propagate"

-- ---------------------------------------------------------------------------
-- Sub-keywords (appear after a block opener, not openers themselves)
-- ---------------------------------------------------------------------------

kwClinch    :: String
kwClinch    = "clinch"

kwWithout   :: String
kwWithout   = "without"

kwMvu       :: String
kwMvu       = "mvu"

-- ---------------------------------------------------------------------------
-- Inline directive keywords
-- ---------------------------------------------------------------------------

kwImply     :: String
kwImply     = "imply"

kwAdd       :: String
kwAdd       = "add"

kwFrom      :: String
kwFrom      = "from"

kwEmit      :: String
kwEmit      = "emit"

kwContextual :: String
kwContextual = "contextual"

kwTinkle :: String
kwTinkle = "tinkle"

-- ---------------------------------------------------------------------------
-- Derived sets
-- ---------------------------------------------------------------------------

kwLet :: String
kwLet = "let"

kwIn :: String
kwIn = "in"

kwFlick :: String
kwFlick = "flick"

-- | All keywords that open a new block (are pushed onto the parser / validator
-- stack).  Must stay in sync with any stack-push logic in Parser.hs.
blockOpeners :: [String]
blockOpeners =
  [ kwRoot, kwChunk, kwCrude, kwDefault, kwOn
  , kwAlien, kwPropagate, kwLet
  ]

-- | The full set of reserved words recognised by @parseHaskellDecl@'s
-- exclusion guard.  A line that starts with any of these (after @\/\/=@) is
-- a template directive, not an inline Haskell declaration.
blockKeywords :: [String]
blockKeywords =
  [ kwDefault, kwOn, kwClinch, kwImply, kwRoot, kwCrude, kwAlien
  , kwPropagate, kwChunk, "include", kwLet, kwIn, kwContextual
  ]

-- | The two stack states in which an @//= on ...@ tag does /not/ push a new
-- frame (it stays within the current default / on group).
onStackContext :: [String]
onStackContext = [kwDefault, kwOn]
