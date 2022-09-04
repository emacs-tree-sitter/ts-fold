[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/ts-fold.svg)](https://jcs-emacs.github.io/jcs-elpa/#/ts-fold)

# ts-fold
> Code-folding using tree-sitter

[![CI](https://github.com/emacs-tree-sitter/ts-fold/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-tree-sitter/ts-fold/actions/workflows/test.yml)

`ts-fold` builds on top of [elisp-tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter)
to provide code folding based on the tree-sitter syntax tree.

<p align="center">
<img src="./etc/screenshot.png" width="80%" height="80%"/>
</p>

<!-- Markdown is not able to render links with unicode so after refreshing the toc, select it and:
    `M-x regexp-replace #[^a-zA-Z] <ret> # <ret>` -->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [ts-fold](#ts-fold)
    - [💾 Installation](#-installation)
        - [🔍 Method 1. with `straight.el` and `use-package`:](#-method-1-with-straightel-and-use-package)
        - [🔍 Method 2. Manual](#-method-2-manual)
    - [📇 Commands](#-commands)
    - [🔨 Supported languages](#-supported-languages)
    - [⚖️ Indicators Mode](#️-indicators-mode)
    - [📝 Summary](#-summary)
    - [🔰 Contribute](#-contribute)
        - [❓ How to create a folding parser?](#-how-to-create-a-folding-parser)
            - [🔍 Where can I look for tree-sitter node?](#-where-can-i-look-for-tree-sitter-node)
            - [🔍 How do I create the function for the corresponding node?](#-how-do-i-create-the-function-for-the-corresponding-node)
            - [🔍 Register in the folding parsers alist!](#-register-in-the-folding-parsers-alist)
        - [❓ How to create a summary parser?](#-how-to-create-a-summary-parser)
            - [🔍 Register to summary parsers alist!](#-register-to-summary-parsers-alist)

<!-- markdown-toc end -->

## 💾 Installation

### 🔍 Method 1. with `straight.el` and `use-package`:

```elisp
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))
```

### 🔍 Method 2. Manual

```sh
git clone https://github.com/emacs-tree-sitter/ts-fold /path/to/lib
```

then in Emacs:

```elisp
(add-to-list 'load-path "/path/to/lib")
(require ts-fold)
```

or

```elisp
(use-package ts-fold
  :load-path "/path/to/lib")
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

> ⚠️ Please sort these two lists alphabetically!

These languages are fairly complete:

* Bash
* C / C++ / C# / CSS
* Elixir
* Go
* HTML
* Java / JavaScript / JSX / JSON / Julia
* Nix
* PHP / Python
* R / Ruby / Rust
* Scala / Swift
* TypeScript / TSX

These languages are in development:

* Agda
* Elm
* Emacs Lisp
* OCaml
* XML (upstream)

## ⚖️ Indicators Mode

<p align="center">
<img src="./etc/indicators.png" width="40%" height=480%"/>
</p>

You need to load `ts-fold-indicators-mode`:
- `use-package`
   ```elisp
   (use-package ts-fold-indicators
   :straight (ts-fold-indicators :type git :host github :repo "emacs-tree-sitter/ts-fold"))
   ```

-
   ```elisp
   (add-to-list 'load-path "/path/to/lib")
   (require ts-fold)
   ```
   or

   ```elisp
   (use-package ts-fold-indicators
      :load-path "/path/to/lib")
   ```

You can then enable this manually by doing the following

```
M-x ts-fold-indicators-mode
```

- To enable this automatically whenever `tree-sitter-mode` is enabled:

   ```el
   (add-hook 'tree-sitter-after-on-hook #ts-fold-indicators-mode)
   ```

- To switch to left/right fringe: (Default is `left-fringe`)

   ```el
   (setq ts-fold-indicators-fringe 'right-fringe)
   ```

- To lower/higher the fringe overlay's priority: (Default is `30`)

   ```el
   (setq ts-fold-indicators-priority 30)
   ```

- To apply different faces depending on some conditions: (Default is `nil`)

  For example, to coordinate [line-reminder](https://github.com/emacs-vs/line-reminder)
with this plugin.

   ```elisp
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

This plugin automatically extracts summary from the comment/document string,
so you can have a nice way to peek at what's inside the fold range.

- If you don't want this to happen, do: (Default is `t`)

   ```elisp
   (setq ts-fold-summary-show nil)
   ```

- Summary are truncated by length: (Default is `60`)

   ```elisp
   (setq ts-fold-summary-max-length 60)
   ```

- The exceeding string are replace by: (Default is `"..."`)

   ```elisp
   (setq ts-fold-summary-exceeded-string "...")
   ```

- To change summary format: (Default is `" <S> %s "`)

   ```elisp
   (setq ts-fold-summary-format " <S> %s ")
   ```

## 🔰 Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

Enable `tree-sitter-mode` first, then `tree-sitter-query-builder` is useful to test
out queries that determine what syntax nodes should be foldable and how to fold
them. [emacs-tree-sitter](https://ubolonton.github.io/emacs-tree-sitter/syntax-highlighting/queries/)
has an excellent documentation on how to write `tree-sitter` queries.

### ❓ How to create a folding parser?

Parsers are defined in the `ts-fold-parsers.el` file. Parser functions are named
with the prefix `ts-fold-parsers-` followed by the `language name`. For example, if
you want to create a parser for the `C` programming language you should name it
`ts-fold-parsers-c`.

Parsers are association lists (alist) whose items consist
of tree-sitter `node` and a function that returns the folding range. See
the following example:

```elisp
(defun ts-fold-parsers-csharp ()
  "Rule sets for C#."
  '((block . ts-fold-range-seq)
    ...))
```

`block` is the tree-sitter node and `ts-fold-range-seq` is the function
that will return the folding range.

Let's move into details,

#### 🔍 Where can I look for tree-sitter node?

To look for the correct node you have three options:
- look at the `tree-sitter-[lang]/grammar.js` implementation. In the above example, `block` node is defined in the [tree-sitter-c-sharp](https://github.com/tree-sitter/tree-sitter-c-sharp)'s
`grammar.js` file
- open a file of your language choice in emacs and `M-x tree-sitter-debug-mode`. This will display the whole s-expr representing your file
- `(message "%S" (tsc-node-to-sexp))` in your function to display what your function is seeing

> ⚠️ Warning
>
> Make sure you look into the correct repository. Repositories are managed
> under [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs)'s
> using git submodule. Some tree-sitter module aren't using the latest version!

#### 🔍 How do I create the function for the corresponding node?

Function take 2 arguments, `node` and `offset`.

* `node` - the targeted tree-sitter node; in this example, `block` will be the
targeting node.
* `offset` - (optional) a cons of two integers. This is handy when you have
a similar rule with little of positioning adjustment.

  `tree-sitter-[lang]` parsers are generally integrated by different authors,
hence their naming and ruling are slightly different (+1/-1 position).

  Let's look at function `ts-fold-range-seq` for better understanding,

   ```elisp
   (defun ts-fold-range-seq (node offset)
     "..."
     (let ((beg (1+ (tsc-node-start-position node)))  ; node beginning position (from Rust layer)
           (end (1- (tsc-node-end-position node))))   ; node end position (from Rust layer)
       (ts-fold--cons-add (cons beg end) offset)))    ; return fold range
   ```

#### 🔍 Register in the folding parsers alist!

Don't forget to add your parser to the entry alist with its corresponding
`major-mode`.

```elisp
(defcustom ts-fold-range-alist
  `((agda-mode       . ,(ts-fold-parsers-agda))
    (sh-mode         . ,(ts-fold-parsers-bash))
    (c-mode          . ,(ts-fold-parsers-c))
    (c++-mode        . ,(ts-fold-parsers-c++))
    ...
```

This variable is defined in package main file, `ts-fold.el`.

### ❓ How to create a summary parser?

`ts-fold-summary.el` module is used to extract and display a short description
from the comment/docstring.

To create a summary parser, you just have to create a function that could
extract comment syntax correctly then register this function to
`ts-fold-summary-parsers-alist` defined in `ts-fold-summary.el`.
The display and shortening will be handled by the module itself.

Functions should be named with the prefix `ts-fold-summary-` followed by `style name`.
For example, to create a summary parser for Javadoc style, then it should be
named `ts-fold-summary-javadoc`.

Let's see the implementation,

```elisp
(defun ts-fold-summary-javadoc (doc-str)
  "..."
  (ts-fold-summary--generic doc-str "*"))  ; strip all asterisks
```

The above summary parser for Javadoc simply remove `*` from any given point.

#### 🔍 Register to summary parsers alist!

Like folding parsers, you should register your summary parser to the entry alist
with its corresponding `major-mode`.

```elisp
(defcustom ts-fold-summary-parsers-alist
  `((actionscript-mode . ts-fold-summary-javadoc)
    (bat-mode          . ts-fold-summary-batch)
    (c-mode            . ts-fold-summary-c)
    (c++-mode          . ts-fold-summary-c)
    ...
```
