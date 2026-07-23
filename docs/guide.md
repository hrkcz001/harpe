# Harpe MVU Template Engine & GHC WASM Integration Guide

Harpe is a lightweight, reactive HTML template engine designed for Haskell applications compiled to WebAssembly (GHC WASM). It implements a strict **Model-View-Update (MVU)** architecture (similar to Elm or PureScript) and supports universal transitions to embed Haskell code directly within HTML.

---

## 1. Syntax & Universal Transitions

Harpe files (`.harpe`) are HTML documents where Haskell expressions, declarations, and types are introduced using universal transitions:

- **Top-Level Haskell Code**: Any code written outside `root`, `chunk`, and `crude` blocks is considered Haskell code by default (no `//=` prefix needed).
- **Inline Transitions (`//= <expr> =//`)**: Used to output Haskell expressions directly in the HTML.
- **Closing Blocks (`=//`)**: Closes reactive `default` or `on` blocks, chunks, or crude blocks.

---

## 2. File Layout

Each `.harpe` template has three distinct logical parts:

### A. Model, Message, & Event Declarations (Top)
Here you define the types of your component's state (`Model`), the interactions it handles (`Msg`), and the notifications it can emit (`Event`).
```haskell
data AppState = AppState { title :: String, clickCount :: Int } deriving (Show)
type Model = AppState

data Msg = Init | Clicked | KeyDown String | InputChanged String | ResetCounts
data Event = ResetDone | EventCaptured String

initModel :: Model
initModel = AppState { title = "WASM Event Studio", clickCount = 0 }
```

### B. HTML View & Reactive Handlers (Middle)
Standard HTML tags mixed with inline Haskell expressions and reactive blocks that respond to state updates and events. Wrapped inside the `root` block.

```html
//= root
<div class="app-root">
  <h2>//= title model =//</h2>
  
  <!-- Reactive Event Block -->
  //= default
    <span class="status-idle">Status: Idle</span>
  //= on EventCaptured desc
    <span class="status-active">Status: //= desc =//</span>
  =//
  
  <button //= onClick Clicked =//>Click Me</button>
</div>
=//
```

### C. Update Function (Bottom)
Updates the model based on the message received, and returns a new model along with a list of events to trigger reactive changes in the view.
```haskell
update :: Msg -> Model -> (Model, [Event])
update Clicked m = (m { clickCount = clickCount m + 1 }, [EventCaptured "Clicked!"])
```

---

## 3. Reactive Blocks (`default` & `on`)

Interactive sections of a template respond to domain events using either a nested `default`/`on` block structure or a standalone `on` block:

### A. Nested Structure
- **`//= default`**: Renders this HTML when no event matching this block has occurred.
- **`//= on <Pattern>`**: Renders this HTML when an event matching the `<Pattern>` occurs.
- **`=//`**: Closes the block structure.

```html
//= default
  <p>No new values.</p>
//= on ValueChanged val
  <p class="success">Value updated to: //= show val =//</p>
=//
```

### B. Standalone `on` Blocks
If you don't need to render any HTML by default (meaning the fallback is empty), you can write `on` blocks directly without a preceding `default` tag:
```html
//= on ValueChanged val
  <p class="success">Value updated to: //= show val =//</p>
=//
```

---

## 4. Component Composition (Clinches)

You can nest components inside other templates using:
```html
//= clinch Child.harpe =//
```

### How the Compiler Handles Nesting:
1. **Model Composition**: Automatically creates a composed Model containing the parent model and the sub-component models:
   ```haskell
   data Model = Model 
     { parentModel :: ParentModel
     , childModel  :: Child.Model 
     } deriving (Show)
   ```
2. **Message & Event Wrappers**: Wraps sub-component messages and events under custom constructors:
   ```haskell
   data AppMsg = MsgParent ParentMsg | MsgChild Child.CounterMsg
   ```
3. **Rendering & Scope**: The parent `view` receives `composedModel` and passes the child slice (`childModel composedModel`) and qualified message wrappers to `Child.view` automatically.

---

## 5. JavaScript FFI & Event Dispatching

Interaction in the browser is driven by a global `window.dispatch` handler that transfers serialized Haskell messages into the WASM RTS.

### A. Template Compiler Output
Event bindings like `<button //= onClick Clicked =//>` are compiled into:
```html
<button onclick="dispatch('MsgParent Clicked')">Click Me</button>
```

#### Parenthesized/Static Parameterization
If the constructor needs hardcoded constant arguments, wrap it in parentheses:
```html
<button //= onClick (NavigateTo "info") =//>Info Page</button>
```
The compiler detects the parenthesized expression, escapes any internal quotes, and serializes it as a static message constructor (without searching the DOM for input fields):
```html
<button onclick="dispatch('MsgParent (NavigateTo &quot;info&quot;)')">Info Page</button>
```

---

## 6. Template Validation & Compiler Feedback

The Harpe compiler implements early semantic validation to ensure template safety and provide clear error messages:

### A. Balanced Block Checking
The compiler counts block openers (`default`, `on` when standalone, `chunk`, `root`, `crude`) and closers (`=//`) to detect unclosed layout tags.
If a tag is unclosed, the compiler rejects the template before compiling GHC code:
```text
Validation Error: Unbalanced block structures. Please check that all blocks (root, chunk, crude, default, on) are properly closed.
```

### B. Event Binding camelCase Validation
Harpe event bindings inside HTML tags must follow camelCase style (starting with "on" followed by an uppercase letter, e.g., `onClick`, `onKeyDown`).
If a lower-case or snake_case binding is found (e.g. `onclick` or `on_click`), the compiler halts with an explicit error:
```text
Syntax Error: Event binding 'onclick' does not follow the required camelCase format (e.g. 'onClick', 'onKeyDown').
```

---

## 7. Chunks & Subview Functions

Chunks allow you to extract reusable pieces of HTML view layout into custom functions that can accept arguments.

### A. Syntax
A chunk is defined using the `chunk` keyword:
```html
//= chunk statusIndicator color text textColor
  <div style="display: flex; align-items: center; gap: 10px;">
    <span style="background: //= color =//;"></span>
    <span style="color: //= textColor =//;">Status: //= text =//</span>
  </div>
=//
```
You render the chunk by calling it using the `imply` keyword inside an inline expression:
```html
//= imply statusIndicator "#10b981" "Active" "#ffffff" =//
```

### B. Mutual Recursion and Definition Order
Chunks are compiled into Haskell `let` bindings within the scope of the component's `view` function:
- **Any Order**: Because Haskell `let` bindings are mutually recursive, you can define chunks in **any order**.
- **Top-Level Definition**: At the template level, all `chunk` blocks must be defined at the top-level of the `.harpe` file (outside the `root` block).

---

## 8. Advanced Haskell Integration

### A. Imports and Pragmas
* **Imports**: Any top-level import statement (e.g. `import qualified FFI`) is parsed as a module import. Preprocessor extracts these lines and places them at the top of the generated `.hs` file.
* **Pragmas**: You can declare GHC language extensions (e.g. `{-# LANGUAGE ... #-}`) in your `.harpe` file outside any blocks. The compiler automatically prepends them to the absolute top of the generated `.hs` file (before the `module` header).
  ```haskell
  {-# LANGUAGE OverloadedStrings, RecordWildCards #-}
  ```

### B. Splitting MVU Definitions to External Files
If you want to keep your Haskell logic (like complex `update` and `Model` definitions) in a separate Haskell file (e.g., [AppExt.hs](file:///C:/Users/hrkcz001/mind/haskell-wasm/src/AppExt.hs)), you can do so and import it inside the `.harpe` file:

1. **Define the Types & Update in your external file** (e.g., `AppExt.hs`):
   ```haskell
   module AppExt where

   data Model = AppState { title :: String, clickCount :: Int } deriving (Show)
   data Msg = Init | Clicked
   data Event = EventDone

   initModel :: Model
   initModel = AppState "External Model" 0

   update :: Msg -> Model -> (Model, [Event])
   update Clicked m = (m { clickCount = clickCount m + 1 }, [EventDone])
   update _ m = (m, [])
   ```

2. **Reference them in your `.harpe` file** (e.g., `App.harpe`):
   ```haskell
   import qualified AppExt
   type Model = AppExt.Model
   data Msg = Msg AppExt.Msg deriving (Show, Read)
   data Event = Event AppExt.Event deriving (Show, Read)
   initModel = AppExt.initModel
   update = AppExt.update

   //= root
   <div>
     <h1>//= title model =//</h1>
     <button //= onClick Clicked =//>Click</button>
   </div>
   =//
   ```

---

### 9. Side Effects & The `crude` Special Word

Harpe supports executing side effects in the `IO` monad (such as URL navigation or FFI logs) during template rendering safely using the `crude` keyword.

### A. View Representation as Pure Html Monad
Instead of returning a raw string or tuple, the component `view` function is written in the pure `Html` monad:
```haskell
view :: [String] -> Model -> Maybe (Event Model) -> Html ()
```
Layout nodes and sub-views are composed using monadic `do` blocks or standard `>>` binds. This keeps rendering completely pure, lazy, and free of side-effects.

### B. Separated Effects Execution
All side effects inside the template are extracted by the compiler into a separate `effects` function:
```haskell
effects :: [String] -> Model -> Maybe (Event Model) -> IO ()
```
The runtime first pure-renders the view, and then executes the `effects` block matching the active event.

### C. Anonymous Crude Blocks
You can execute raw `IO` statements inside `default` or `on` blocks:
```html
//= on RedirectTo "info"
    //= clinch Info.harpe =//
    //= crude
      imply transform changeUrl "info"
    =//
=//
```
Any FFI side-effects inside anonymous `crude` blocks are extracted and executed within the `effects` function.

### D. Named Crude Blocks
You can declare named crude blocks at the top level (outside `root`), which compiles to top-level `IO ()` declarations:
```haskell
//= crude logMessage msg
  putStrLn msg
=//
```
You invoke the named crude block using the `imply crude` statement within inline transitions:
```html
//= imply crude logMessage "User opened info page" =//
```

---

## 10. JavaScript FFI with `alien` Blocks

To simplify and unify JavaScript integration from Haskell templates, Harpe supports inline FFI blocks and directives directly in `.harpe` files:

### A. Inline FFI Definition (`//= alien transformer`)
You can define custom JavaScript FFI blocks directly inside your templates. The compiler compiles these into dedicated GHC WASM JS FFI imports and Haskell wrapper functions:
```html
//= alien transformer changeUrl url
  window.history.pushState({}, '', //= url =//)
=//
```
- **Compiled to WASM**: `foreign import javascript unsafe "jsCode" js_name :: JSString -> IO ()` plus a wrapper `name :: String -> IO ()` converting arguments using `toJSString`.
- **Compiled to Native**: `name :: String -> IO ()` returning `return ()` (enabling normal local compilation and tests!).

### B. Shared FFI Imports (`//= add alien <target> from`)
You can define FFI blocks in a shared `.harpe` file (e.g. `Aliens.harpe`) and import only specific FFI declarations recursively using the `add alien` directive:
- **By Name**: `//= add alien changeUrl [as otherName] from Aliens.harpe =//` imports only the FFI block named `changeUrl`, optionally renaming it locally to `otherName`.
- **All Informers**: `//= add alien informer from Aliens.harpe =//` imports all FFI blocks declared as `alien informer` (or `propagate alien informer`).
- **All Transformers**: `//= add alien transformer from Aliens.harpe =//` imports all FFI blocks declared as `alien transformer` (or `propagate alien transformer`).

The compiler recursively resolves all imported templates and automatically deduplicates alien blocks by their function name so that each FFI call is generated only once.

### C. FFI safety inside crude blocks (`imply alien`)
Unsafe FFI side effects must be wrapped in a `crude` block (anonymous or named) and prefixed with `imply alien` (e.g. `imply alien changeUrl "home"`):
```html
//= on RedirectTo "home"
    //= crude
      imply alien changeUrl "home"
    =//
=//
```

### F. Contextual Alien Lifecycle (`imply contextual alien`)

Harpe supports **automatic cleanup** of DOM event listeners (aliens) when `default/on` blocks transition between states or when a parent component unmounts the block entirely.

#### Syntax
Mark an alien call inside an `on` block as **contextual**:
```html
//= on KeyDownListener True
    //= crude
      imply contextual alien keylistener
    =//
=//
```

#### How it works
When the compiler detects `imply contextual alien` inside an `on` block:
1. It generates a **top-level `IORef [String]`** to track the previous branch's active alien IDs.
2. Each `on` block returns `([String], IO ())` tuples via `on_effects_ctx` — the alien IDs + the effects.
3. `default_effects_ctx` compares the previous branch's IDs with the current branch's IDs. **Only when they differ** does it call the **tinkle** cleanup function for each old ID — preventing unnecessary remove-re-add cycles.
4. The tinkle cleanup is defined inline inside the alien block declaration using the `tinkle` keyword separator.

#### Defining tinkle cleanup inside an alien block

The `tinkle` section is placed after the alien body, separated by a line containing only `tinkle`:
```html
//= propagate alien keylistener
  const handler = (event) => { inform('KeyDown', event.key); };
  document.addEventListener('keydown', handler);
  window.__keylistener_handler = handler;
tinkle
  if (window.__keylistener_handler) {
    document.removeEventListener('keydown', window.__keylistener_handler);
    delete window.__keylistener_handler;
  }
=//
```

The alien body JS code runs when the alien is activated; the tinkle JS code runs on scope transition. Tinkle takes **no arguments from Haskell** — it's pure JS `IO ()`.

#### How the compiler processes tinkle
1. The alien block is parsed with an optional `tinkle` section producing `AlienBlock name args bodyLines (Just tinkleLines)` or `(Nothing)` if absent.
2. `compileAlien` generates two FFI functions:
   - `<name>` — the main body (JS FFI, may receive Haskell args)
   - `<name>_tinkle` — the cleanup body (JS FFI, `IO ()`, no args)
3. When compiling a `DefaultBlock` with contextual aliens, the compiler generates a tinkle lookup lambda:
   ```haskell
   \name -> case name of { "keylistener" -> keylistener_tinkle; _ -> return () }
   ```
   Only aliens that actually have a `tinkle` section are included in the lambda.
4. `default_effects_ctx` calls `tinkleFn prevId` for each previously-active alien ID when the scope transitions.

#### Example

**Aliens.harpe** (shared FFI module):
```html
//= without mvu ==/

//= propagate alien keylistener
  const handler = (event) => { inform('KeyDown', event.key); };
  document.addEventListener('keydown', handler);
  window.__keylistener_handler = handler;
tinkle
  if (window.__keylistener_handler) {
    document.removeEventListener('keydown', window.__keylistener_handler);
    delete window.__keylistener_handler;
  }
=//
```

**App.harpe** (uses contextual imply):
```html
//= add alien keylistener from Aliens.harpe

//= default
//= on KeyDownListener False
    <div>Inactive</div>
//= on KeyDownListener True
    <div>Active</div>
    //= crude
      imply contextual alien keylistener
    =//
=//
```

When `KeyDownListener True` fires, `keylistener` runs (adds the event listener). When the state transitions away (e.g. `KeyDownListener False`), `keylistener_tinkle` runs (removes the event listener).

#### Explicit tinkle call
Tinkle functions can also be called directly from crude blocks:
```html
//= crude
  keylistener_tinkle
=//
```

#### Aliens without tinkle
If an alien has no `tinkle` section, no cleanup runs on scope transition. This is fine for one-shot side effects like URL navigation or HTTP fetch.



### D. Direct FFI Event Binding (`//= onClick alien`)
To write lightweight templates without manually declaring component messages or writing custom update rules, you can bind FFI calls directly to HTML element event handlers:
```html
<button //= onClick alien changeUrl "info" =//>Info Page</button>
```
The template compiler automatically translates direct FFI bindings into self-propagating `MsgAlien` / `EventAlien` packets, which bubble up child levels and execute the side-effect at the top-level parent component.

### E. Declarative Listeners (`//= alien informer` & `inform`)
Instead of writing manual DOM listeners inside javascript FFI blocks, you can declare global event listeners directly in the template using the `alien informer` keyword:
```html
//= alien informer keydown event
  inform('KeyDown', event.key)
=//
```
- **`inform` Sugar Function**: A globally defined `window.inform(ctorName, ...args)` helper handles event serialization, making it easy to dispatch parameterized messages back to GHC WASM without manual string interpolation.
- **Startup Execution**: Since the compiler generates a safe Haskell `keydown :: IO ()` action, you can trigger listener registration on startup inside a `crude` block using `imply inform`:
  ```html
  //= on EventCaptured "WASM RTS Initialized"
      //= crude
        imply inform keydown
        imply inform input
      =//
  =//
  ```
