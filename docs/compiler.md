# Harpe Compiler & Parser — Technical Specification

This document covers the parser pipeline, AST construction, two-pass codegen architecture,
clinch composition, FFI mapping, and the event-bubbling compilation model.

---

## 1. Module Structure

The compiler is split into focused sub-modules. No module exceeds ~350 lines.

| Module | Responsibility |
|---|---|
| `Harpe.Parser` | Template pre-processing, validation, combinator parsing → `[TemplateNode]` |
| `Harpe.Parser.Combinators` | `Parser` newtype and primitive combinators |
| `Harpe.Syntax` | DSL keyword & delimiter constants (leaf — no Harpe.* imports) |
| `Harpe.CodeGen.Names` | Generated identifier naming conventions (leaf — no Harpe.* imports) |
| `Harpe.Compiler` | Thin orchestrator — `compileTemplate`; alien collection; output assembly |
| `Harpe.Compiler.Common` | File IO helpers (`readUtf8File`), `replaceWordBoundaries`, `replaceSubstring` |
| `Harpe.Compiler.Alien` | JS FFI codegen (`compileAlien`, `compileInformer`) |
| `Harpe.Compiler.NodeUtils` | `partitionNodes`, `processCrudeLine`, `nextBlockId`, `collectIncludes` |
| `Harpe.Compiler.PatternValidator` | `on`-block pattern validation (`validatePattern`, `wrapPatternForChild`) |
| `Harpe.Compiler.ViewGen` | Pure `Html ()` view-pass codegen (`compileNodesWithDeclsView`) |
| `Harpe.Compiler.EffectsGen` | `IO ()` effects-pass codegen (`compileNodesWithDeclsEffects`, `extractContextualAlienIds`, `ctxAccumFetch`) |
| `Harpe.Templates` | All user-facing error / warning message strings |

---

## 2. Preprocessor & Parsing Pipeline (`Harpe.Parser`)

### A. Entry point

```haskell
parseTemplate :: String -> Either String TemplateAST
```

Full pipeline: `validateTemplateText` → `preprocess` → `runParser (parseNodes <* eof) body`.

### B. `preprocess`

```haskell
preprocess :: String -> ([String], [String], String)
--                        imports   decls    body
```

Walks lines top-to-bottom tracking an open-block **stack**:

- **Stack empty + `import …`** → extracted to `imports`, line blanked in body.
- **Stack empty + plain Haskell** (no `//=`, no `<`) → extracted to `declarations`, line blanked.
- **Stack empty + `//= root/chunk/crude/…`** → push keyword, line stays.
- **Stack non-empty** → line stays; adjust stack on openers / closers.

### C. Validation passes (before parsing)

| Check | Description |
|---|---|
| `checkSeparateLines` | Block openers must appear on their own line |
| `checkBalancedBlocks` | Every `//=` opener has a matching `=//` |
| `checkEventNames` | `onXxx` attributes must be camelCase (`onClick`, `onKeyDown`, …) |

### D. Block parsers (tried in order inside `parseNode`)

```
parseChunkBlock               //= chunk name args … =//
parseRootBlock                //= root … =//
parseDefaultBlock             //= default [/ on Pat] … [//= on Pat …] =//
parseStandaloneOnBlocks       //= on Pat … =//  (without default prefix)
parseCrudeBlock               //= crude [name args] … =//
parseAlienBlock               //= alien transformer name args … =//
parseInformerBlock            //= alien informer name [type] … =//
parsePropagatedAlienBlock     //= propagate alien transformer … =//
parsePropagatedInformerBlock  //= propagate alien informer … =//
parseAddAlien                 //= add alien fn from File.harpe =//
parsePropagateAlien           //= propagate alien transformer name =//  (ref only)
parsePropagateInformer        //= propagate alien informer name =//    (ref only)
parseImplyInformer            //= imply inform name =//
parseImplyAlien               //= imply transform name args =//
parseImplyCrude               //= imply crude name args =//
parseImply                    //= imply … =//  (generic → HaskellExpr)
parseWithoutDirective         //= without mvu ==/
parseHaskellDecl              //= someExpr  (no =// — single line, not a block keyword)
parseClinchTemplate           //= clinch File.harpe [handler] =//
parseEventBinding             //= onEvent args =//  (inside HTML attributes)
parseInlineExpr               //= expr =//  (inside HTML text nodes)
parseHtmlChunk                everything else until next //= or =//
```

### E. Keyword constants (`Harpe.Syntax`)

All string literals for delimiters and block keywords live in `Harpe.Syntax` (a leaf module):

```haskell
symOpen   = "//="   -- block/inline opener
symClose  = "=//'"  -- block closer
kwRoot    = "root"
kwChunk   = "chunk"
kwCrude   = "crude"
kwDefault = "default"
kwOn      = "on"
-- … plus blockOpeners :: [String], blockKeywords :: [String]
```

---

## 3. Compiler Orchestrator (`Harpe.Compiler`)

### Main entry point

```haskell
compileTemplate :: FilePath -> String -> TemplateAST -> IO String
--                 sourcePath  moduleName   ast
```

### Assembly steps

1. **Rename parent declarations** — `replaceWordBoundaries` rewrites `Msg/Model/Event` →
   `ParentMsg/ParentModel/ParentEvent` at word boundaries; `initModel` → `initParentModel`.
2. **Partition nodes** — `partitionNodes` splits the flat node list into 7 buckets:
   `chunks`, `mbRoot`, `crudeDecls`, `localAliens`, `adds`, `props`, `others`.
3. **Collect aliens** — `collectAliens` resolves every `AddAlien target path` by reading and
   parsing the referenced `.harpe` file, then extracting the matching alien block by name.
   `nubByAlienName` deduplicates so the same function name is only emitted once.
4. **Collect includes** — `collectIncludes` walks the AST recursively for all `ClinchTemplate`
   nodes, returning a list of `IncludeInfo { includeModuleName, includeFieldName, includeHandlerName }`.
5. **Two-pass codegen** — `compileNodesWithDeclsView` and `compileNodesWithDeclsEffects` both
   walk the body nodes. Nodes are split at `HaskellDecl` boundaries:
   - Non-decl runs → `expr1 >> expr2 >> …`
   - A `HaskellDecl` → `(let x = … in (rest))`
6. **Assemble view body** — chunks become `let { … }` bindings prepended to the main view
   expression.
7. **Assemble output** — `data`, `type`, `instance`, helper declarations and the boilerplate
   module template are concatenated into the final `.hs` source string.

### Identifier naming conventions (`Harpe.CodeGen.Names`)

All generated Haskell names are defined as constants in the leaf module `Harpe.CodeGen.Names`:

```haskell
nmParentMsg   = "ParentMsg"
nmParentModel = "ParentModel"
nmParentEvent = "ParentEvent"
nmMsgAlien    = "MsgAlien"
nmEventAlien  = "EventAlien"
rtComposedModel = "composedModel"
rtActiveEvent   = "activeEvent"
domBlockPrefix  = "harpe-block-"
mkEventCtor modName = "Event" ++ modName   -- EventCounter, EventInfo, …
mkMsgCtor   modName = "Msg"   ++ modName
-- …
```

---

## 4. View Pass (`Harpe.Compiler.ViewGen`)

`compileNodesWithDeclsView` delegates per-node compilation to `compileNodeView`:

| Node | Generated expression |
|---|---|
| `HtmlChunk s` | `writeHtml "<escaped>"` |
| `HaskellExpr expr` | `toHtml (expr)` or verbatim for `imply …` chunk calls |
| `EventBinding name args` | `writeHtml (" onclick=\"" ++ dispatchJS … ++ "\"")` |
| `ClinchTemplate path handler` | `Harpe.Core.view msgWrappers (childModel composedModel) routedEvent` |
| `DefaultBlock pats defNodes onBlocks` | `default_view blockId defExpr handler activeEvent` |
| `ChunkBlock name args nodes` | Emits a `let name = \a1 a2 -> <body>` binding |

`blockId` values are globally unique per-render: `"harpe-block-ModuleName-N"` generated by
`nextBlockId` — an `IORef Int` counter incremented once per `DefaultBlock`.

---

## 5. Effects Pass (`Harpe.Compiler.EffectsGen`)

`compileNodesWithDeclsEffects` delegates per-node compilation to `compileNode`:

| Node | Generated expression |
|---|---|
| `HtmlChunk`, `HaskellExpr`, `EventBinding` | *(nothing — filtered out)* |
| `CrudeBlock Nothing [] body` | Each line run through `processCrudeLine`; joined with `>>` |
| `ImplyCrude name args` | `name arg1 arg2` |
| `ClinchTemplate` | Routes `effects` call to child module with routed sub-event |
| `DefaultBlock` | `default_effects handler activeEvent` |

### `processCrudeLine` (NodeUtils)

Guards alien names inside crude blocks. Every word that is a known alien name must be
preceded by `imply alien` or `imply contextual alien`. Bare alien names are a compile error.
The prefix is stripped, leaving the raw function call.

### Contextual alien compilation (`EffectsGen`)

When a `DefaultBlock` contains `imply contextual alien` in any nested `on` block:

1. **`extractContextualAlienIds`** — Recursively walks the AST across both `defNodes` and `onBlocks`
   to collect all contextual alien names. The recursive traversal covers `DefaultBlock`,
   `OnBlock`, `CrudeBlock`, `AnonBlock`, `ConstBlock`, `RootBlock`, and `ChunkBlock`.
2. **`tinkleNames` collection** — `Compiler.hs` collects the set of alien names that have a `tinkle`
   section from the collected alien blocks. This is passed to `EffectsGen` as a `[String]` parameter.
   Tinkle bodies are NOT compiled separately — they are embedded in the alien FFI via `compileAlien`
   which generates both `<name>` and `<name>_tinkle` FFI functions.
3. **IORef generation** — A unique `IORef [String]` is generated per block via `mkCtxIORefName`
   (e.g. `ctxTracker_App_0`). Declarations are accumulated in `ctxAccum` (an `IORef [String]`)
   and emitted at the module top level by `Compiler.hs`.
4. **Tinkle lambda** — A generated function `\name -> case name of { "keylistener" -> keylistener_tinkle; _ -> return () }`
   maps each contextual alien name to its compiled tinkle IO action. Only aliens that actually have
   a `tinkle` section are included. Aliens without tinkle fall through to `return ()`.
5. **`compileOnBlockCtx`** — Each on-block is compiled to return `Just ([String], IO ())`:
   - The `[String]` is the on-block's own contextual IDs (from `extractContextualAlienIds bodyNodes`)
   - On-blocks without contextual IDs fall back to the block's `allCtxIds` (union of all IDs)
6. **`default_effects_ctx`** — The runtime function accepts `(String -> IO ())` as its
   tinkle lookup parameter (simpler than the old `Maybe (IO ())`):
   - Reads previous IDs: `prevCtx <- readIORef ref`
   - Calls `tinkleFn name` for each previous ID when `prevCtx /= currCtx`
7. **Backward compatibility** — `DefaultBlock` without contextual aliens uses the original
   `default_effects` / `on_effects` API unchanged. No tinkle bodies are emitted.

### Alien dispatch (top-level effects)

The top-level `effects` in the generated `Component` instance handles `EventAlien` separately:

```haskell
effects msgWrappers composedModel activeEvent =
  case activeEvent of
    Just (EventAlien func args) -> case (func, args) of
      ("changeUrl", [a1]) -> changeUrl a1
      ("httpFetch",  …  ) -> httpFetch …
      _ -> return ()
    _ -> <compiled effects tree>
```

---

## 6. Component Composition (Clinch)

When `//= clinch Counter.harpe handleCounterEvent =//` is found:

### Model composition

```haskell
data Model = Model
  { parentModel  :: ParentModel
  , counterModel :: Counter.Model
  } deriving Show
```

### Message & event wrappers

```haskell
data AppMsg   = MsgParent ParentMsg   | MsgAlien T.Text [T.Text] | MsgCounter Counter.CounterMsg
data AppEvent = EventParent ParentEvent | EventAlien T.Text [T.Text] | EventCounter Counter.CounterEvent
```

### Update delegation

```haskell
update (MsgCounter cMsg) model =
  let (nextChild, cEvents) = Counter.update cMsg (counterModel model)
      cAlien = [ e | e@(Counter.EventAlien _ _) <- cEvents ]
      cOther = [ e | e <- cEvents, not (isAlien e) ]
      parentMsgs = concatMap
        (\e -> case handleCounterEvent e of
                 Just m  -> [MsgParent m]
                 Nothing -> []) cOther
  in foldl … (model { counterModel = nextChild }, []) parentMsgs
```

If no handler is given, the `concatMap` part is `[]` — child events are silently dropped
(except `EventAlien`, which always bubbles to the top).

---

## 7. Pattern Validation (`Harpe.Compiler.PatternValidator`)

On-block patterns (`ValueChanged val`, `RedirectTo "info" / Done`) are validated before codegen:

1. **`splitRespectingParens`** — tokenises the pattern string respecting parenthesis depth.
2. **First token** must start with an uppercase letter (constructor name).
3. **Each argument** must be one of:
   - Wildcard `_`
   - String literal `"…"`
   - Numeric literal (all digits)
   - Bool literal `True` / `False`
   - Variable name `lowerIdent`
   - Variable with type `lowerIdent :: TypeName`
   - Nested constructor `(Ctor arg…)`
4. **Multi-patterns** (`Pat1 / Pat2`) are split on `/` and each part validated separately.
5. **Auto-wrapping**: if the first token is not already a known child event constructor
   (`EventParent`, `EventCounter`, …), `wrapPatternForChild` prefixes with `EventParent (…)`.

---

## 8. FFI Alien System (`Harpe.Compiler.Alien`)

### `compileAlien` — transformer blocks

```haskell
foreign import javascript unsafe "<js>" js_name :: JSString -> JSString -> IO ()
```

- Arg placeholders `//= argName =//` in the JS body → `$1`, `$2`, … via `replaceArgPlaceholders`.
- Wrapped in `#if wasm32_HOST_ARCH` … `#else return () #endif` for native stubs.
- Emits a Haskell wrapper converting `String` → `JSString` via `toJSString`.

### `compileInformer` — informer blocks

```haskell
foreign import javascript unsafe
  "document.addEventListener('keydown', (event) => { inform('KeyDown', event.key) })"
  js_informer_keydown :: IO ()
```

`window.inform(ctorName, …args)` serialises and dispatches messages back into WASM without
manual string interpolation.

### Sharing (`add alien`)

```harpe
//= add alien changeUrl from Aliens.harpe =//
```

The compiler reads `Aliens.harpe`, parses it, finds the matching `AlienBlock` or `InformerBlock`
by name, and copies its definition. The import `import Aliens (changeUrl)` is injected into the
generated module header.

---

## 9. Generated Module Structure

A standard MVU component compiles to:

```haskell
{-# LANGUAGE CPP, OverloadedStrings #-}
module Foo ( Model, initModel, Foo.update, decodeMsg, stepRenderWire, runWireMessages
           , ParentModel, initParentModel, parentUpdate
           , ParentMsg(..), ParentEvent(..), FooMsg(..), FooEvent(..), DomEnv ) where

import qualified Harpe.Core
import Harpe.Core ( Html, DomEnv, Component(..), … )
import qualified Counter        -- per clinch
import Counter (CounterEvent)
-- alien imports, template imports

-- Hoisted + renamed Haskell from template body
type ParentModel = …
data ParentMsg   = …
data ParentEvent = …
initParentModel  = …
parentUpdate :: ParentMsg -> ParentModel -> (ParentModel, [ParentEvent])

-- Composed types
data Model    = Model { parentModel :: ParentModel, counterModel :: Counter.Model }
data FooMsg   = MsgParent ParentMsg | MsgAlien T.Text [T.Text] | MsgCounter Counter.CounterMsg
data FooEvent = EventParent ParentEvent | EventAlien T.Text [T.Text] | EventCounter Counter.CounterEvent

-- Runtime helpers (generated)
initModel       :: Model
update          :: FooMsg -> Model -> (Model, [FooEvent])
decodeMsg       :: String -> Either String FooMsg
stepRenderWire  :: DomEnv -> Maybe String -> Model -> Either String (Model, String, IO ())
runWireMessages :: DomEnv -> [String] -> Model -> Either String (Model, String, IO ())

instance Component Model where
  type Msg   Model = FooMsg
  type Event Model = FooEvent
  update = Foo.update
  view msgWrappers composedModel activeEvent =
    case activeEvent of
      Just (EventAlien _ _) -> Harpe.Core.view msgWrappers composedModel Nothing
      _ -> let { model = composedModel; statusDot color = … }
           in <compiled view tree>
  effects msgWrappers composedModel activeEvent =
    case activeEvent of
      Just (EventAlien func args) -> case (func, args) of { … }
      _ -> <compiled effects tree>
```
