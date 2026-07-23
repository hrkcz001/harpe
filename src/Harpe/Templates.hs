module Harpe.Templates where

-- | Fixed GHC pragmas always emitted for every compiled module.
moduleFixedPragmas :: [String]
moduleFixedPragmas =
  [ "{-# LANGUAGE TypeFamilies #-}"
  , "{-# LANGUAGE CPP #-}"
  , "{-# LANGUAGE MultiParamTypeClasses #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE BlockArguments #-}"
  , "{-# OPTIONS_GHC -Wno-overlapping-patterns #-}"
  ]

-- | The full `import Harpe.Core (...)` import line used in every MVU module.
harpeCoreMvuImport :: String
harpeCoreMvuImport =
  "import Harpe.Core (ToHtml(..), Component, default_view, default_effects, default_effects_ctx, on_view, on_effects, on_effects_ctx, wrapMsg, dispatchJS, writeHtml, toHtml, renderHtml, Html(..), Err, runMessages, imply, DomEnv)\nimport qualified Data.Text as T\n"

-- | The minimal `import qualified Harpe.Core` used in without-mvu modules.
harpeCoreMinImport :: String
harpeCoreMinImport = "import qualified Harpe.Core\nimport qualified Data.Text as T\n"

-- | CPP / FFI imports injected when a module contains alien FFI definitions.
ffiPreamble :: [String]
ffiPreamble =
  [ ""
  , "#if defined(wasm32_HOST_ARCH)"
  , "import GHC.Wasm.Prim (JSVal, JSString(..), toJSString, fromJSString)"
  , "#endif"
  ]

-- ---------------------------------------------------------------------------
-- Compile-error message helpers
-- All human-facing error strings live here so they can be updated in one place.
-- The templates/messages.toml is the canonical reference — keep in sync.
--
-- Code ranges:
--   E0xx  — Validation errors (caught before parsing)
--   E1xx  — Parse errors
--   E2xx  — Compiler / codegen errors
-- ---------------------------------------------------------------------------

-- ── E0xx — Validation errors ────────────────────────────────────────────────

-- | E001 — block opener not on its own line
errValidationSeparateLine :: Int -> String
errValidationSeparateLine lineNo =
  "harpe E001 — line " ++ show lineNo ++ ": block opener must be on its own separate line.\n" ++
  "  A '//= default', '//= chunk', '//= crude', '//= alien' etc. opener cannot share\n" ++
  "  a line with HTML content or closing tags.\n" ++
  "  Fix: move the block opener to its own line."

-- | E002 — block closer not on its own line
errValidationSeparateCloser :: Int -> String
errValidationSeparateCloser lineNo =
  "harpe E002 — line " ++ show lineNo ++ ": block closer '=//' must be on its own separate line.\n" ++
  "  Put '=//' on its own line with no other content."

-- | E003 — unbalanced block structure
errValidationUnbalanced :: String
errValidationUnbalanced =
  "harpe E003 — unbalanced block structure.\n" ++
  "  One or more blocks were opened but never closed.\n" ++
  "  Check that every //= root / chunk / crude / default / alien has a matching =// closer."

-- | E004 — bad event name format
errValidationEventName :: String -> String
errValidationEventName name =
  "harpe E004 — event binding '" ++ name ++ "' is not valid camelCase.\n" ++
  "  Event bindings must start with 'on' followed by an uppercase letter.\n" ++
  "  Examples: onClick, onKeyDown, onMouseEnter\n" ++
  "  Got: '" ++ name ++ "'"

-- ── E1xx — Parse errors ─────────────────────────────────────────────────────

-- | E101 — unexpected EOF
errParseUnexpectedEof :: String -> String
errParseUnexpectedEof target =
  "harpe E101 — unexpected end of file while looking for '" ++ target ++ "'.\n" ++
  "  Check that the block starting before this point is properly closed with =//."

-- | E102 — expected word boundary after keyword
errParseWordBoundary :: String -> String
errParseWordBoundary kw =
  "harpe E102 — expected a word boundary after keyword '" ++ kw ++ "'.\n" ++
  "  Add a space after the keyword."

-- | E103 — generic parse failure
errParseFailed :: String -> String
errParseFailed detail =
  "harpe E103 — template parse failed: " ++ detail ++ "\n" ++
  "  Check for malformed //= ... =// directives near the indicated position."

-- ── E2xx — Compiler / codegen errors ────────────────────────────────────────

-- | E201 — alien called without imply guard in a crude block
errAlienUnsafeCall :: String -> String
errAlienUnsafeCall name =
  "harpe E201 — unsafe FFI call to alien '" ++ name ++ "' outside a guarded context.\n" ++
  "  Inside crude blocks, alien functions must be called via:\n" ++
  "    imply alien " ++ name ++ " <args>\n" ++
  "  Direct bare calls to '" ++ name ++ "' bypass the safety wrapper.\n" ++
  "  Wrap the call with 'imply alien'."
-- | E203 — imply alien used outside a crude block
errImplyTransformOutsideCrude :: String -> [String] -> String
errImplyTransformOutsideCrude name args =
  "harpe E203 — 'imply alien " ++ name ++ " " ++ unwords args ++ "' is not allowed outside of crude blocks.\n" ++
  "  'imply alien' is only valid inside a //= crude ... =// block.\n" ++
  "  Move this line into a crude block.\n" ++
  "  Example:\n" ++
  "    //= crude\n" ++
  "        imply alien " ++ name ++ " " ++ unwords args ++ "\n" ++
  "    =//\n"

-- | E204 — alien source file not found
errAlienFileNotFound :: FilePath -> String
errAlienFileNotFound path =
  "harpe E204 — alien source file not found: " ++ path ++ "\n" ++
  "  The '//= add alien ... from File.harpe =//'\n" ++
  "  directive expects the referenced file to exist.\n" ++
  "  Check the path is relative to the current template's directory."

-- | E205 — alien function not found inside the referenced file
errAlienNotFound :: String -> FilePath -> String
errAlienNotFound target path =
  "harpe E205 — alien function '" ++ target ++ "' not found in " ++ path ++ ".\n" ++
  "  Make sure the function is declared with:\n" ++
  "    //= alien " ++ target ++ " ...  =//\n" ++
  "  or\n" ++
  "    //= propagate alien " ++ target ++ " ...  =//\n" ++
  "  in that file."

-- | E206 — invalid pattern argument form
errPatternArg :: String -> String -> String
errPatternArg arg rawPat =
  "harpe E206 — invalid argument '" ++ arg ++ "' in pattern '" ++ rawPat ++ "'.\n" ++
  "  Allowed argument forms:\n" ++
  "    1. Wildcard              _\n" ++
  "    2. String literal        \"info\"\n" ++
  "    3. Numeric literal       42\n" ++
  "    4. Bool literal          True / False\n" ++
  "    5. Variable              key\n" ++
  "    6. Typed variable        (key :: String)\n" ++
  "    7. Nested constructor    (Ctor arg1 ...)\n" ++
  "  Got: '" ++ arg ++ "' in pattern '" ++ rawPat ++ "'\n" ++
  "  Hint: if you meant a variable, make sure it starts with a lowercase letter."

-- | E207 — pattern constructor must start with uppercase (or empty pattern)
errPatternInvalidCtor :: String -> String
errPatternInvalidCtor rawPat =
  "harpe E207 — invalid pattern in '//= on " ++ rawPat ++ " =//':\n" ++
  "  Constructor must start with an uppercase letter.\n" ++
  "  Examples:  //= on ValueChanged val =//\n" ++
  "             //= on Done =//\n" ++
  "  Got: '" ++ rawPat ++ "'"

errPatternEmpty :: String
errPatternEmpty =
  "harpe E207 — empty pattern in '//= on ... =//'.\n" ++
  "  Provide a constructor name, e.g. //= on Done =// or //= on ValueChanged val =//."

-- | E208 — named crude block in view context
errCrudeInvalidContext :: String -> String
errCrudeInvalidContext name =
  "harpe E208 — named crude block '" ++ name ++ "' used in invalid context.\n" ++
  "  Named crude blocks (//= crude name args ... =// ) are top-level declarations.\n" ++
  "  They cannot appear nested inside view nodes.\n" ++
  "  Move the crude block to the top level of the template."

-- | E209 — parse error in referenced alien file
errAlienFileParseError :: FilePath -> String -> String
errAlienFileParseError path err =
  "harpe E209 — failed to parse alien file '" ++ path ++ "':\n  " ++ err

-- | E210 — clinch template processing error
errClinchProcessing :: FilePath -> String -> String
errClinchProcessing path detail =
  "harpe E210 — clinch template '" ++ path ++ "' could not be processed:\n  " ++ detail

-- ── Internal errors (bugs, should never reach the user in normal use) ────────

-- | Compiler bug: a node that should have been partitioned out was reached in a pass
errInternalAlienNode :: String -> String
errInternalAlienNode nodeType =
  "harpe internal error — " ++ nodeType ++ " node reached a pass where it should have been partitioned out.\n" ++
  "  This is a bug in the Harpe compiler. Please report it.\n" ++
  "  Workaround: check that all alien/propagate blocks are at the top level of your template."

-- | Compiler bug: compileChunk called with a non-ChunkBlock node
errNotAChunkBlock :: String
errNotAChunkBlock = "harpe internal error - compileChunk called with a non-ChunkBlock node. This is a compiler bug."

-- ── Infrastructure / setup errors ────────────────────────────────────────────

-- | W001 — templates directory not found at runtime (falls back to './templates')
errTemplatesDirNotFound :: String
errTemplatesDirNotFound = "harpe warning: could not locate templates directory, using './templates' as fallback"

-- | E211 — module_boilerplate.hs template file not found
errBoilerplateNotFound :: FilePath -> String
errBoilerplateNotFound path =
  "harpe error: module_boilerplate.hs template not found at " ++ path ++
  "\n  This file is required to compile MVU modules." ++
  "\n  Reinstall harpe or check your templates/ directory."
