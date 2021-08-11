[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CI](https://github.com/jcs090218/tree-sitter-fold/actions/workflows/test.yml/badge.svg)](https://github.com/jcs090218/tree-sitter-fold/actions/workflows/test.yml)

# tree-sitter-fold
> Code-folding using tree-sitter

tree-sitter-fold builds on top of [elisp-tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter)
to provide code folding base on the tree-sitter syntax tree.

## :floppy_disk: Installation

#### Methods 1. with `straight.el` and `use-package`:

```el
(use-package tree-sitter-fold
  :straight (host github repo "junyi-hou/tree-sitter-fold"))
```

#### Methods 2. Manual

```sh
git clone https://github.com/jcs090218/tree-sitter-fold /path/to/lib
```

then in Emacs:

```el
(add-to-list 'load-path "/path/to/lib")
(require 'tree-sitter-fold)
```

## :card_index: Usage

| Commands                          | Description                                                                 |
|-----------------------------------|-----------------------------------------------------------------------------|
| tree-sitter-fold-close            | fold the current syntax node.                                               |
| tree-sitter-fold-open             | open all folds inside the current syntax node.                              |
| tree-sitter-fold-open-recursively | open the outmost fold of the current syntax node. Keep the sub-folds close. |
| tree-sitter-fold-close-all        | close all foldable syntax nodes in the current buffer.                      |
| tree-sitter-fold-open-all         | open all folded syntax nodes in the current buffer.                         |

## :hammer: Supported languages

* Go
* Nix
* Python
* R

## Contribution

Enable `tree-sitter-mode` first, then `tree-sitter-query-builder` is useful to test
out queries that determine what syntax nodes should be foldable and how to fold
them. [emacs-tree-sitter](https://ubolonton.github.io/emacs-tree-sitter/syntax-highlighting/queries/)
has an excellent documentation on how to write `tree-sitter` queries.
