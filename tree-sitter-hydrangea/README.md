# tree-sitter-hydrangea

Tree-sitter grammar for Hydrangea.

The grammar is derived from the compiler's source-language definition in:

- `src/Language/Hydrangea/Lexer.x`
- `src/Language/Hydrangea/Parser.y`
- `src/Language/Hydrangea/Syntax.hs`

## Development

```bash
cd tree-sitter-hydrangea
npm install
npm run generate
npm test
```

For a quick smoke test against real source files:

```bash
cd tree-sitter-hydrangea
npm run parse:examples
```

## Notes

- The grammar includes an external scanner for Hydrangea's nested `(* ... *)` comments.
- Corpus tests cover focused syntax cases, while `parse:examples` exercises representative `.hyd` programs from the main repository.

## Emacs

`editors/emacs/hydrangea-ts-mode.el` provides an Emacs 29+ major mode built on top of this tree-sitter grammar.

Add the mode to your `load-path`, load it, and install the grammar from this checkout once:

```elisp
(add-to-list 'load-path "/path/to/hydrangea-compiler/tree-sitter-hydrangea/editors/emacs")
(require 'hydrangea-ts-mode)
(hydrangea-ts-mode-install-grammar)
```

After that, `.hyd` files open in `hydrangea-ts-mode` automatically.
