[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/ts-fold.svg)](https://jcs-emacs.github.io/jcs-elpa/#/ts-fold)

# ts-fold
> Code-folding using tree-sitter

[![CI](https://github.com/jcs-elpa/ts-fold/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/ts-fold/actions/workflows/test.yml)

ts-fold builds on top of [elisp-tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter)
to provide code folding base on the tree-sitter syntax tree.

<p align="center">
<img src="./etc/screenshot.png" width="80%" height="80%"/>
</p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [ts-fold](#ts-fold)
    - [💾 Installation](#💾-installation)
        - [🔍 Methods 1. with `straight.el` and `use-package`:](#🔍-methods-1-with-straightel-and-use-package)
        - [🔍 Methods 2. Manual](#🔍-methods-2-manual)
    - [📇 Commands](#📇-commands)
    - [🔨 Supported languages](#🔨-supported-languages)
    - [⚖️ Indicators Mode](#⚖️-indicators-mode)
    - [📝 Summary](#📝-summary)
    - [🔰 Contribute](#🔰-contribute)
        - [How to write a parser?](#how-to-write-a-parser)
            - [Where can I look for tree-sitter node?](#where-can-i-look-for-tree-sitter-node)
            - [How do I create the function for the corresponding node?](#how-do-i-create-the-function-for-the-corresponding-node)
            - [Register to parsers alist!](#register-to-parsers-alist)

<!-- markdown-toc end -->

## 💾 Installation

### 🔍 Methods 1. with `straight.el` and `use-package`:

```el
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "jcs-elpa/ts-fold"))
```

### 🔍 Methods 2. Manual

```sh
git clone https://github.com/jcs-elpa/ts-fold /path/to/lib
```

then in Emacs:

```el
(add-to-list 'load-path "/path/to/lib")
(require ts-fold)
```

## 📇 Commands

| Commands                   | Description                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `ts-fold-close`            | fold the current syntax node.                                               |
| `ts-fold-open`             | open all folds inside the current syntax node.                              |
| `ts-fold-open-recursively` | open the outmost fold of the current syntax node. Keep the sub-folds close. |
| `ts-fold-close-all`        | close all foldable syntax nodes in the current buffer.                      |
| `ts-fold-open-all`         | open all folded syntax nodes in the current buffer.                         |
| `ts-fold-toggle`           | toggle the syntax node at `point'.                                          |

## 🔨 Supported languages

> These languages are fairly complete:

* Bash
* C / C++ / C# / CSS
* Go
* HTML
* Java / JavaScript / JSX / JSON
* Nix
* PHP / Python
* R / Ruby / Rust
* Scala / Swift
* TypeScript / TSX

> These languages are in development:

* Agda
* Elm
* Elixir
* Emacs Lisp
* XML (upstream)

## ⚖️ Indicators Mode

<p align="center">
<img src="./etc/indicators.png" width="40%" height=480%"/>
</p>

You can enable this manually by doing the folloiwng

```
M-x ts-fold-indicators-mode
```

To enable this automatically whenever `tree-sitter-mode` is enabled:

```el
(add-hook 'tree-sitter-after-on-hook #ts-fold-indicators-mode)
```

To switch to left/right fringe: (Default is `left-fringe`)

```el
(setq ts-fold-indicators-fringe 'right-fringe)
```

To lower/higher the fringe overlays: (Default is `30`)

```el
(setq ts-fold-indicators-priority 30)
```

To apply different face depends on some conditions: (Default is `nil`)

For example, to coordinate [line-reminder](https://github.com/emacs-vs/line-reminder)
with this plugin.

```el
(setq ts-fold-indicators-face-function
      (lambda (pos &rest _)
        (let ((ln (line-number-at-pos pos)))
          (cond
           ((memq ln line-reminder--change-lines) 'line-reminder-modified-sign-face)
           ((memq ln line-reminder--saved-lines) 'line-reminder-saved-sign-face)
           (t nil)))))
```

## 📝 Summary

<p align="center">
<img src="./etc/summary.gif" width="80%" height="80%"/>
</p>

This plugin automatically extract summary from the comment/document string,
so you can have a nice way to peek what's inside the fold range.

If you don't want this to happen, do: (Default is `t`)

```el
(setq ts-fold-summary-show nil)
```

Summary are truncated by length: (Default is `60`)

```el
(setq ts-fold-summary-max-length 60)
```

The exceeding string are replace by: (Default is `"..."`)

```el
(setq ts-fold-summary-exceeded-string "...")
```

To change summary format: (Default is `" <S> %s "`)

```el
(setq ts-fold-summary-format " <S> %s ")
```

## 🔰 Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

Enable `tree-sitter-mode` first, then `tree-sitter-query-builder` is useful to test
out queries that determine what syntax nodes should be foldable and how to fold
them. [emacs-tree-sitter](https://ubolonton.github.io/emacs-tree-sitter/syntax-highlighting/queries/)
has an excellent documentation on how to write `tree-sitter` queries.

### How to write a parser?

Parsers are ruled in the `ts-fold-parsers.el` file. Parser function follow
with the prefix `ts-fold-parsers-` plus the `language name`. For example, if
you want to create a parser for `C` programming language. It should be named:
`ts-fold-parsers-c`.

The parser is consist of an association list (alist), each item is consist
of tree-sitter `node` and a function that returns the folding range. See
the following example:

```elisp
(defun ts-fold-parsers-csharp ()
  "Rule sets for C#."
  '((block . ts-fold-range-seq)
    ...))
```

`block` is the tree-sitter node, and `ts-fold-range-seq` is the function
that will return the folding range.

Let's move into details,

#### Where can I look for tree-sitter node?

To look for the correct node, you should look at the `tree-sitter-[lang]/grammar.js`
implementation. In the above example, `block` node is defined in the
[tree-sitter-c-sharp](https://github.com/tree-sitter/tree-sitter-c-sharp)'s
`grammar.js` file.

> ⚠️ Warning
>
> Make sure you look into the correct repository. Repositories are managed
> under [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs)'s
> using the git submodule. Some tree-sitter module aren't using the latest version!

#### How do I create the function for the corresponding node?

Function takes 2 arguments, `node` and `offset`.

* `node` - the targeting tree-sitter node; in this example, `block` will be the
targeting node.
* `offset` - (optiona) a cons consist of two integers. This is handy when you have
a similar rule with little of positioning adjustment.

`tree-sitter-[lang]` parsers are generally integrated with different authors,
hence their naming and ruling are slightly different (+1/-1 position).

Let's look at function `ts-fold-range-seq` for better understanding,

```elisp
(defun ts-fold-range-seq (node offset)
  "..."
  (let ((beg (1+ (tsc-node-start-position node)))  ; node beginning position (from Rust layer)
        (end (1- (tsc-node-end-position node))))   ; node end position (from Rust layer)
    (ts-fold--cons-add (cons beg end) offset)))    ; return fold range
```

#### Register to parsers alist!

Don't get to add your parsers to entry alist.

```elisp
(defcustom ts-fold-range-alist
  `((agda-mode       . ,(ts-fold-parsers-agda))
    (sh-mode         . ,(ts-fold-parsers-bash))
    (c-mode          . ,(ts-fold-parsers-c))
    (c++-mode        . ,(ts-fold-parsers-c++))
    ...
```

This variable is defined in package main file, `ts-fold.el`.
