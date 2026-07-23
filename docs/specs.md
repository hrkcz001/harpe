# Harpe Technical Specification Index

This document serves as the high-level technical index and entry point for the **Harpe** reactive template engine specification. The complete details are split into dedicated, specialized modules:

---

## 1. [Syntax & Grammar Specification](file:///C:/Users/hrkcz001/mind/harpe/docs/grammar.md)
* **Transitions**: Document-level HTML and Haskell boundaries using the `//=` and `=//` delimiters.
* **Block Structures**: Grammar definitions for `root`, `default`, `on`, combined `default / on`, `crude`, and `chunk` nodes.
* **TextMate Highlighting**: Rules, regex patterns, scopes, and publisher token styles in [harpe.tmLanguage.json](file:///C:/Users/hrkcz001/mind/harpe-syntax/syntaxes/harpe.tmLanguage.json).

---

## 2. [Compiler & Parser Specification](file:///C:/Users/hrkcz001/mind/harpe/docs/compiler.md)
* **Lexical Parser Combinators**: Pipeline in `Harpe.Parser` to pre-process templates and validate syntax rules (such as legacy code rejection and balanced block checks).
* **ModuleClincherComposition**: Composing parent-child components, aggregating state record properties, and wrapping child messages under parent event namespaces.
* **Bubbling and FFI Mapping**: Compiling automatic event mapping via the `Bubble` class, bubbling FFI calls, and translating inline JS FFI blocks.

---

## 3. [Runtime & Core Architecture Specification](file:///C:/Users/hrkcz001/mind/harpe/docs/runtime.md)
* **Pure monadic HTML builder**: The `Html` reader+writer monad carries a read-only DOM snapshot (`DomEnv`) and a difference-list buffer for O(1) string concatenation.
* **Persistent DOM State Cache**: Maintaining element rendering attributes and contents inside the global `window.harpe_blocks` cache.
* **Routing State Transitions**: Distinguishing layout transitions (`"default"`) from event messages (`"on"`) via `Maybe (Html (), String)`.
* **Nested Effects Propagation**: Compiling side-effects inside default blocks.
* **Layout-Independent Code Emission**: Generating case clauses using curly braces `{ ... }` and semicolons `;`.
