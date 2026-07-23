# Harpe Runtime & Core Architecture Specification

This document details the core runtime environment [Harpe.Core](file:///C:/Users/hrkcz001/mind/harpe-core/src/Harpe/Core.hs), the pure `Html` monad, the FFI DOM state cache, and the layout-independent code emission rules.

---

## 1. Pure Monadic Architecture

The view rendering cycle is entirely pure — no IO is needed during rendering. All DOM side-effects are separated out by the compiler into a dedicated `effects` function and executed by the runtime after rendering completes.

### A. The `Html` Environment Reader
The `Html` builder is implemented as a pure reader+writer monad. It carries a read-only snapshot of the DOM state (`DomEnv`) alongside a difference-list `ShowS` buffer for O(1) string concatenation:
```haskell
type DomEnv = [(String, (String, String))]
newtype Html a = Html { runHtml :: DomEnv -> ShowS -> (a, ShowS) }
```
This enables O(1) string concatenations and pure state lookups.

### B. Pure Rendering Loop
The FFI DOM querying is entirely sequenced within GHC WASM's `IO` monad *before* the rendering loop begins. The compiler-generated WASM entry point (e.g. `renderFromWire`) fetches the latest DOM snapshot using `getDomEnv` and passes it as a pure parameter to `stepRenderWire`:
```haskell
renderFromWire mbRaw = do
  model <- readIORef appStore
  env <- getDomEnv
  case stepRenderWire env mbRaw model of
    Left err -> ...
    Right (nextModel, htmlString, ioAction) -> do
      writeIORef appStore nextModel
      updateDOM "app" htmlString
      ioAction
```

---

## 2. FFI DOM State Cache (`window.harpe_blocks`)

Because components can be nested or conditionally unrendered (e.g., when a parent switches active layouts), rendering blocks may temporarily be removed from the active DOM. To prevent their display state (`data-harpe-view` attribute and HTML contents) from resetting to default values, the runtime maintains a persistent cache.

### A. Persistent JavaScript Cache
The JS FFI implementation in `Core.hs` manages block states within the global `window.harpe_blocks` object:
```javascript
(() => {
  window.harpe_blocks = window.harpe_blocks || {};
  
  // 1. Sync any blocks currently rendered in the DOM to the persistent cache
  for (const el of document.querySelectorAll('[id^="harpe-block-"]')) {
    window.harpe_blocks[el.id] = {
      html: el.innerHTML,
      view: el.getAttribute('data-harpe-view') || ''
    };
  }
  
  // 2. Format and return the entire persistent cache
  const res = [];
  for (const id in window.harpe_blocks) {
    const data = window.harpe_blocks[id];
    res.push(id + '\x1f' + data.html + '\x1f' + data.view);
  }
  return res.join('\x1e');
})()
```

### B. Separator-Based Record Parsing
To remain completely lightweight and avoid GHC JSON parsing overhead, `getDomEnv` uses flat records separated by control characters `\x1e` (record separator) and `\x1f` (unit separator). `Core.hs` parses these record segments purely:
```haskell
parseDomEnv :: String -> DomEnv
parseDomEnv raw =
  [ (id', (html', attr'))
  | record <- splitOn '\x1e' raw
  , let parts = splitOn '\x1f' record
  , length parts == 3
  , let id'   = parts !! 0
        html' = parts !! 1
        attr' = parts !! 2
  ]
```

---

## 3. Combined Block State Transitions (`(Html (), String)`)

To distinguish between rendering a temporary event override (an `on` state) and returning to a dynamic main layout (a `default` state), the handler callback returns a tuple carrying the view type:
```haskell
default_view :: String -> Html () -> (Maybe event -> Maybe (Html (), String)) -> Maybe event -> Html ()
```
* **Event Blocks (`"on"`)**:
  * Set `data-harpe-view="on"`.
  * If the active event shifts to an unhandled message, they freeze and preserve the `currentHtml` in the DOM.
* **Layout Blocks (`"default"`)**:
  * Set `data-harpe-view="default"`.
  * If the active event changes, they do not freeze; instead, they safely evaluate `runH defHtml` to dynamically re-render the layout with updated model values.

---

## 4. Layout-Independent GHC Emission

To avoid indentation and layout parsing conflicts when GHC WASM compiles nested monadic statements, the compiler generates case expressions using explicit curly braces `{ ... }` and semicolons `;`:
```haskell
default_view "harpe-block-App-0" ( ... ) (\mb -> on_view (\event -> case event of { EventParent (RedirectTo "info") -> Just ((Harpe.Core.view ...), "on"); EventParent (RedirectTo "home") -> Just (writeHtml ..., "default"); _ -> Nothing }) mb) activeEvent
```
This guarantees layout-independent and robust syntax compilation.

---

## 5. Nested Effects Propagation

`compileNode` in `EffectsGen` handles `DefaultBlock` by composing the default-branch expression with the event-routed effects call.

### Standard (no contextual aliens)
```haskell
defExpr >> default_effects (\mb -> on_effects (\event -> case event of { ... }) mb) activeEvent
```

### With contextual aliens and tinkle cleanup
When `imply contextual alien` is detected, the compiler generates:
```haskell
-- Top-level IORef (emitted once per block at the module level)
ctxTracker_App_0 :: IORef [String]
ctxTracker_App_0 = unsafePerformIO (newIORef [])
{-# NOINLINE ctxTracker_App_0 #-}

-- In the effects method:
defExpr >>
default_effects_ctx ctxTracker_App_0
  (\name -> case name of { "keylistener" -> keylistener_tinkle; _ -> return () })
  (\mb -> on_effects_ctx (\event -> case event of
    EventParent (KeyDownListener True) -> Just (["keylistener"], do keylistener)
    EventParent (RedirectTo "home") -> Just (["keylistener"], return ())
    _ -> Nothing) mb)
  activeEvent
```

The tinkle lambda `\name -> ...` is generated from the `tinkle` sections inside alien block declarations.
`default_effects_ctx` reads the previous alien IDs from the IORef, calls `tinkleFn name` for each
old ID when `prevCtx /= currCtx`, and updates the IORef with the new branch's IDs.

### `default_effects_ctx` tinkle-aware signature

```haskell
default_effects_ctx
  :: IORef [String]
  -> (String -> IO ())  -- tinkle lookup: name → cleanup IO (always returns ())
  -> (Maybe event -> Maybe ([String], IO ()))
  -> Maybe event -> IO ()
```

The tinkle function `String -> IO ()` is generated at compile time from the `tinkle` section of
each alien block. Aliens without a `tinkle` section fall through to `return ()`.
