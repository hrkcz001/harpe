# Harpe Grammar & Syntax Highlighting Specification

This document provides the full language syntax design and VS Code TextMate highlighting specification for Harpe MVU templates.

---

## 1. Syntax Transitions & Lexical Rules

Harpe files (`.harpe`) are standard HTML files that support transitions to pure Haskell code using the `//=` and `=//` delimiters.

### A. Core Transition Delimiters
* **Inline Transitions**: Formed using `//=` and `=//` on the same line. For example:
  ```html
  <h1>//= title model =//</h1>
  ```
  The compiler extracts the Haskell code between `//=` and `=//`, compiling it into a string expression (which is subsequently passed to the HTML writer).
* **Block Transitions**: Any line starting with `//=` that does *not* contain a closing `=//` on the same line begins a block structure (such as `default`, `on`, `root`, `chunk`, or `crude`). The block remains open until closed by a line containing only `=//`.
* **Top-Level Code**: Any line outside of layout block definitions (i.e. outside `root`, `chunk`, and `crude`) is treated as top-level Haskell code by default. This allows developers to write imports, helper functions, and state records directly in the template.

### B. Block Structure Grammar
* **`root` Block**:
  * Demarcated by `//= root` and `=//` on separate lines.
  * Defines the main layout view function of the component.
* **`default` Block**:
  * Demarcated by `//= default` and `=//` on separate lines.
  * Contains the HTML template rendered when no active event message is being processed.
* **`on <Pattern>` Block**:
  * Demarcated by `//= on <Pattern>` and `=//` on separate lines.
  * **Nested**: Declared inside `default` blocks to override rendering when a specific event pattern matches.
  * **Standalone**: Declared directly in root/chunk layout to match events where no fallback HTML is needed.
* **Combined `default / on <Pattern>` Block**:
  * Demarcated by `//= default / on <Pattern>` and `=//`.
  * Used to share the same default HTML layout when a specific routing event (e.g. `RedirectTo "home"`) occurs, preventing code duplication.
* **`crude` Block**:
  * Demarcated by `//= crude` or `//= crude <name> <args>`.
  * Serves as `do` notation for side-effects in the `IO` monad.
* **`chunk <name> <args>` Block**:
  * Demarcated by `//= chunk <name> <args>` and `=//`.
  * Defines reusable subview template helper functions.

---

## 2. TextMate Highlight Scopes (`harpe.tmLanguage.json`)

To support the `.harpe` extension in VS Code, a custom TextMate grammar file [harpe.tmLanguage.json](file:///C:/Users/hrkcz001/mind/harpe-syntax/syntaxes/harpe.tmLanguage.json) is used.

### A. Root Scopes
* **Default Language Scope**: The root scope is set to `source.haskell`, meaning all top-level declarations are automatically highlighted as standard Haskell code.
* **HTML Embeds**: Block openers (`root`, `chunk`) transition the syntax scope into `text.html.derivative`, where standard HTML tagging is applied.

### B. Block Injection Hierarchy
TextMate uses the `injections` property to target patterns inside HTML blocks without modifying the parent HTML grammar:
1. **`harpe-default-on-block`**:
   * **Pattern**: `^\s*(//=)\s*(default)\s*(/)\s*(on)\b`
   * **Scopes**:
     * `default` and `on` -> `keyword.control.harpe` (purple bold).
     * `/` -> `constant.character.escape.harpe` (yellow italic).
     * `//=` -> `comment.line.harpe` (gray).
2. **`harpe-default-block`**:
   * **Pattern**: `^\s*(//=)\s*(default)\b`
   * **Scopes**:
     * `default` -> `keyword.control.harpe` (purple).
3. **`harpe-standalone-on-block`**:
   * **Pattern**: `^\s*(//=)\s*(on)\b`
   * **Scopes**:
     * `on` -> `keyword.control.harpe` (purple).
4. **`harpe-imply`**:
   * **Pattern**: `(//=)\s*(imply)\b`
   * **Scopes**:
     * `//=` -> `constant.character.escape.harpe`.
     * `imply` -> `keyword.control.harpe`.
     * **Sub-pattern `\b(crude|alien|contextual)\b`** -> `keyword.control.harpe`.
5. **`haskell-block`**:
   * **Pattern**: `^\s*(//=)(?!.*=//)`
   * **Scopes**:
     * Entire rest of the line -> `meta.embedded.block.haskell` (delegates to Haskell).
     * Inner keywords (`default`, `on`, `clinch`, `root`, `chunk`, `crude`, `alien`, `add`, `propagate`, `contextual`, `tinkle`) -> `keyword.control.harpe`.
     * Pattern separators (`/`) -> `constant.character.escape.harpe`.
     * The `imply` regex now accepts `(imply)\s+(?:(contextual)\s+)?(crude\s+|alien\s+)?([a-zA-Z0-9_']+)` — the optional `contextual` group highlights as `keyword.control.harpe` between `imply` and `alien`.

### C. Styling Map
VS Code token colors are customized in `package.json` under `configurationDefaults`:
* `comment.line.harpe`: Foreground `#555555` (muted gray for delimiters).
* `keyword.control.harpe`: Foreground `#8B5CF6`, bold (purple for active blocks).
* `entity.name.function.harpe`: Foreground `#bb5385`, bold (pink for chunk/crude names).
* `constant.character.escape.harpe`: Foreground `#ced412`, italic (yellow italic for pattern separators and inline transitions).

---

## 3. Modular Grammar Compilation System

To ensure that the 900+ lines TextMate grammar file is easy to maintain, the source files are split and compiled programmatically.

### A. Source Directory Structure
The repository rules are located inside [harpe-syntax/src](file:///C:/Users/hrkcz001/mind/harpe-syntax/src):
* `src/base.json`: Contains the top-level configuration metadata (`name`, `scopeName`, `patterns`, `injections`, `contributes`).
* `src/rules/*.json`: Each TextMate scope rule in the `repository` is placed in its own individual, named JSON file (e.g. `harpe-default-block.json`, `haskell-block.json`, etc.).

### B. Compiler Script
The compilation is managed by a Node.js script [build.js](file:///C:/Users/hrkcz001/mind/harpe-syntax/build.js) located in the extension root:
* It reads `base.json`, iterates through the `src/rules/` directory to read and parse all rule JSON files, aggregates them into the `repository` object, and generates the final, unified [syntaxes/harpe.tmLanguage.json](file:///C:/Users/hrkcz001/mind/harpe-syntax/syntaxes/harpe.tmLanguage.json) file.
* Developers can compile the grammar at any time by running:
  ```bash
  npm run build
  ```
