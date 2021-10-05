;;; tree-sitter-fold-summary.el --- Extract summary from fold region  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-10-04 16:59:22

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extract summary from fold region.
;;

;;; Code:

(require 's)

(defcustom tree-sitter-fold-summary-show t
  "Flag to show summary if available."
  :type 'boolean
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-summary-max-length 60
  "Maximum length for summary to display."
  :type '(choice (const :tag "nil" nil)
                 (integer :tag "positive integer number"))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-summary-exceeded-string "..."
  "String that added after display summary.
This happens only when summary length is larger than variable
`tree-sitter-fold-summary-max-length'."
  :type 'string
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-summary-format " <S> %s "
  "Prefix string added before summary overlay."
  :type 'string
  :group 'tree-sitter-fold)

;;
;; (@* "Externals" )
;;

(defvar tree-sitter-fold-replacement-face)

;;
;; (@* "Parsers" )
;;

(defun tree-sitter-fold-summary--valid-content-p (content)
  "Return non-nil if CONTENT is a valid document string for extraction.
Some programmers use some type of characters for splitting the code module
into sections.  For instance, ===, ---, ///, =-=, etc.  Try to omit these
type of content by checking the word boundary's existence."
  (string-match-p "\\w" content))

(defun tree-sitter-fold-summary--apply-sym (line sym)
  "Remove SYM from LINE."
  (when (string-prefix-p sym line)
    (setq line (substring line (length sym) (length line))
          line (string-trim line)))
  line)

(defun tree-sitter-fold-summary--extract-summary (doc-str sym)
  "Extract only document summary from DOC-STR using SYM"
  (let ((lines (split-string doc-str "\n")) new-lines)
    (dolist (line lines)
      (setq line (string-trim line))
      (cond ((listp sym)
             (dolist (c sym) (setq line (tree-sitter-fold-summary--apply-sym line c))))
            (t (setq line (tree-sitter-fold-summary--apply-sym line sym))))
      (when (tree-sitter-fold-summary--valid-content-p line) (push line new-lines)))
    (reverse new-lines)))

(defun tree-sitter-fold-summary--doc-extract (doc-str sym)
  "Default way to extract the doc summary from DOC-STR."
  (let* ((lines (tree-sitter-fold-summary--extract-summary doc-str sym)) (summary (nth 0 lines)))
    (when summary (setq summary (string-trim summary)))
    (if (string-empty-p summary) nil summary)))

(defun tree-sitter-fold-summary--generic (doc-str sym)
  "Generic DOC-STR extraction using SYM."
  (when (tree-sitter-fold-util--doc-faces-p doc-str)
    (tree-sitter-fold-summary--doc-extract doc-str sym)))

(defun tree-sitter-fold-summary-batch (doc-str)
  "Extract batch summary from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str '("::" "rem" "REM")))

(defun tree-sitter-fold-summary-csharp-vsdoc (doc-str)
  "Extract C# vsdoc summary from DOC-STR."
  (let ((type-triple (string-match-p "///" doc-str)))
    (setq doc-str (s-replace-regexp "<[/]*[^>]+." "" doc-str))
    (tree-sitter-fold-summary--generic doc-str (if type-triple "///" "//"))))

(defun tree-sitter-fold-summary-csharp (doc-str)
  "Extract summary for C# from DOC-STR."
  (cond ((string-match-p "///" doc-str)
         (tree-sitter-fold-summary-csharp-vsdoc doc-str))
        (t (tree-sitter-fold-summary-javadoc doc-str))))

(defun tree-sitter-fold-summary-javadoc (doc-str)
  "Extract javadoc summary from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "*"))

(defun tree-sitter-fold-summary-go (doc-str)
  "Extract Go document summary from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "//"))

(defun tree-sitter-fold-summary-lua-doc (doc-str)
  "Extract Lua document string from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "--"))

(defun tree-sitter-fold-summary-python-doc (doc-str)
  "Extract Python document string from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "\"\"\""))

(defun tree-sitter-fold-summary-ruby-doc (doc-str)
  "Extract Ruby document string from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "#"))

(defun tree-sitter-fold-summary-rust-doc (doc-str)
  "Extract Rust document summary from DOC-STR."
  (tree-sitter-fold-summary--generic doc-str "///"))

(defun tree-sitter-fold-summary-c-macro (doc-str)
  "Parse C macro summary from DOC-STR."
  (when (tree-sitter-fold-util--is-face doc-str
                                        '(font-lock-preprocessor-face
                                          preproc-font-lock-preprocessor-background))
    (tree-sitter-fold-summary--doc-extract doc-str "")))

(defun tree-sitter-fold-summary-c (doc-str)
  "Summary parser for C from DOC-STR."
  (or (tree-sitter-fold-summary-javadoc doc-str)
      (tree-sitter-fold-summary-c-macro doc-str)))

(defun tree-sitter-fold-summary-markdown (doc-str)
  "Extract Makrdown block from DOC-STR."
  (tree-sitter-fold-summary--doc-extract doc-str '()))

(defun tree-sitter-fold-summary-org (doc-str)
  "Extract Org block from DOC-STR."
  (tree-sitter-fold-summary--doc-extract doc-str '()))

;;
;; (@* "Core" )
;;

(defun tree-sitter-fold-summary--keep-length (summary)
  "Keep the SUMMARY length to `tree-sitter-fold-summary-max-length'."
  (let ((len-sum (length summary))
        (len-exc (length tree-sitter-fold-summary-exceeded-string)))
    (when (< tree-sitter-fold-summary-max-length len-sum)
      (setq summary (substring summary 0 (- tree-sitter-fold-summary-max-length len-exc))
            summary (concat summary tree-sitter-fold-summary-exceeded-string))))
  summary)

(defun tree-sitter-fold-summary--apply-format (summary)
  "Return the SUMMARY that has added the summary prefix."
  (format tree-sitter-fold-summary-format summary))

(defun tree-sitter-fold-summary--parser ()
  "Return the summary parser from `tree-sitter-fold-summary-parsers-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) tree-sitter-fold-summary-parsers-alist))

(defun tree-sitter-fold-summary--get (doc-str)
  "Extract summary from DOC-STR in order to display ontop of the overlay."
  (let ((parser (cdr (tree-sitter-fold-summary--parser))) summary)
    (when parser
      (setq summary (funcall parser doc-str))
      (when (integerp tree-sitter-fold-summary-max-length)
        (setq summary (tree-sitter-fold-summary--keep-length summary)))
      (when summary
        (setq summary (tree-sitter-fold-summary--apply-format summary)
              summary (propertize summary 'face 'tree-sitter-fold-replacement-face))))
    summary))

(defcustom tree-sitter-fold-summary-parsers-alist
  `((actionscript-mode . tree-sitter-fold-summary-javadoc)
    (bat-mode          . tree-sitter-fold-summary-batch)
    (c-mode            . tree-sitter-fold-summary-c)
    (c++-mode          . tree-sitter-fold-summary-c)
    (csharp-mode       . tree-sitter-fold-summary-csharp)
    (go-mode           . tree-sitter-fold-summary-go)
    (java-mode         . tree-sitter-fold-summary-javadoc)
    (javascript-mode   . tree-sitter-fold-summary-javadoc)
    (js-mode           . tree-sitter-fold-summary-javadoc)
    (js2-mode          . tree-sitter-fold-summary-javadoc)
    (js3-mode          . tree-sitter-fold-summary-javadoc)
    (kotlin-mode       . tree-sitter-fold-summary-javadoc)
    (lua-mode          . tree-sitter-fold-summary-lua-doc)
    (markdown-mode     . tree-sitter-fold-summary-markdown)
    (objc-mode         . tree-sitter-fold-summary-c)
    (org-mode          . tree-sitter-fold-summary-org)
    (php-mode          . tree-sitter-fold-summary-javadoc)
    (python-mode       . tree-sitter-fold-summary-python-doc)
    (rjsx-mode         . tree-sitter-fold-summary-javadoc)
    (ruby-mode         . tree-sitter-fold-summary-ruby-doc)
    (rust-mode         . tree-sitter-fold-summary-rust-doc)
    (scala-mode        . tree-sitter-fold-summary-javadoc)
    (sh-mode           . tree-sitter-fold-summary-javadoc)
    (swift-mode        . tree-sitter-fold-summary-c)
    (typescript-mode   . tree-sitter-fold-summary-javadoc))
  "Alist mapping major-mode to doc parser function."
  :type 'hook
  :group 'tree-sitter-fold)

(provide 'tree-sitter-fold-summary)
;;; tree-sitter-fold-summary.el ends here
