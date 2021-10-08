[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# tree-sitter-fold
> Code-folding using tree-sitter

[![CI](https://github.com/jcs090218/tree-sitter-fold/actions/workflows/test.yml/badge.svg)](https://github.com/jcs090218/tree-sitter-fold/actions/workflows/test.yml)

tree-sitter-fold builds on top of [elisp-tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter)
to provide code folding base on the tree-sitter syntax tree.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [tree-sitter-fold](#tree-sitter-fold)
    - [💾 Installation](#💾-installation)
        - [🔍 Methods 1. with `straight.el` and `use-package`:](#🔍-methods-1-with-straightel-and-use-package)
        - [🔍 Methods 2. Manual](#🔍-methods-2-manual)
    - [📇 Commands](#📇-commands)
    - [🔨 Supported languages](#🔨-supported-languages)
    - [🔰 Contribution](#🔰-contribution)
    - [⚖️ Indicators Mode](#⚖️-indicators-mode)
    - [📝 Summary](#📝-summary)

<!-- markdown-toc end -->

## 💾 Installation

### 🔍 Methods 1. with `straight.el` and `use-package`:

```el
(use-package tree-sitter-fold
  :straight (host github repo "jcs090218/tree-sitter-fold"))
```

### 🔍 Methods 2. Manual

```sh
git clone https://github.com/jcs090218/tree-sitter-fold /path/to/lib
```

then in Emacs:

```el
(add-to-list 'load-path "/path/to/lib")
(require 'tree-sitter-fold)
```

## 📇 Commands

| Commands                            | Description                                                                 |
|-------------------------------------|-----------------------------------------------------------------------------|
| `tree-sitter-fold-close`            | fold the current syntax node.                                               |
| `tree-sitter-fold-open`             | open all folds inside the current syntax node.                              |
| `tree-sitter-fold-open-recursively` | open the outmost fold of the current syntax node. Keep the sub-folds close. |
| `tree-sitter-fold-close-all`        | close all foldable syntax nodes in the current buffer.                      |
| `tree-sitter-fold-open-all`         | open all folded syntax nodes in the current buffer.                         |
| `tree-sitter-fold-toggle`           | toggle the syntax node at `point'.                                          |

## 🔨 Supported languages

> These languages are fairly complete:

* Bash
* C / C++ / C# / CSS
* Go
* Java / JavaScript / JSX / JSON
* Nix
* PHP / Python
* R / Rust
* Scala / Swift
* TypeScript / TSX

> These languages are in development:

* Agda
* HTML
* Ruby

## 🔰 Contribution

Enable `tree-sitter-mode` first, then `tree-sitter-query-builder` is useful to test
out queries that determine what syntax nodes should be foldable and how to fold
them. [emacs-tree-sitter](https://ubolonton.github.io/emacs-tree-sitter/syntax-highlighting/queries/)
has an excellent documentation on how to write `tree-sitter` queries.

## ⚖️ Indicators Mode

You can enable this manually by doing the folloiwng

```
M-x tree-sitter-fold-indicators-mode
```

To enable this automatically whenever `tree-sitter-mode` is enabled:

```el
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-fold-indicators-mode)
```

To switch to left/right fringe: (Default is `left-fringe`)

```el
(setq tree-sitter-fold-indicators-fringe 'right-fringe)
```

To lower/higher the fringe overlays: (Default is `30`)

```el
(setq tree-sitter-fold-indicators-priority 30)
```

To apply different face depends on some conditions: (Default is `nil`)

For example, to coordinate [line-reminder](https://github.com/emacs-vs/line-reminder)
with this plugin.

```el
(setq tree-sitter-fold-indicators-face-function
      (lambda (pos &rest _)
        (let ((ln (line-number-at-pos pos)))
          (cond
           ((memq ln line-reminder--change-lines) 'line-reminder-modified-sign-face)
           ((memq ln line-reminder--saved-lines) 'line-reminder-saved-sign-face)
           (t nil)))))
```

## 📝 Summary

This plugin automatically extract summary from the comment/document string,
so you can have a nice way to peek what's inside the fold range.

If you don't want this to happen, do: (Default is `t`)

```el
(setq tree-sitter-fold-summary-show nil)
```

Summary are truncated by length: (Default is `60`)

```el
(setq tree-sitter-fold-summary-max-length 60)
```

The exceeding string are replace by: (Default is `"..."`)

```el
(setq tree-sitter-fold-summary-exceeded-string "...")
```

To change summary format: (Default is `" <S> %s "`)

```el
(setq tree-sitter-fold-summary-format " <S> %s ")
```
