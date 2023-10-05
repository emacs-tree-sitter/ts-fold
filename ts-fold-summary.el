;;; ts-fold-summary.el --- Extract summary from fold region  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Shen, Jen-Chieh
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

(defcustom ts-fold-summary-show t
  "Flag to show summary if available."
  :type 'boolean
  :group 'ts-fold)

(defcustom ts-fold-summary-max-length 60
  "Maximum length for summary to display."
  :type '(choice (const :tag "nil" nil)
                 (integer :tag "positive integer number"))
  :group 'ts-fold)

(defcustom ts-fold-summary-exceeded-string "..."
  "String that added after display summary.
This happens only when summary length is larger than variable
`ts-fold-summary-max-length'."
  :type 'string
  :group 'ts-fold)

(defcustom ts-fold-summary-format " <S> %s "
  "Prefix string added before summary overlay."
  :type 'string
  :group 'ts-fold)

;;
;; (@* "Externals" )
;;

(defvar ts-fold-replacement-face)

;;
;; (@* "Parsers" )
;;

(defun ts-fold-summary--valid-content-p (content)
  "Return non-nil if CONTENT is a valid document string for extraction.
Some programmers use some type of characters for splitting the code module
into sections.  For instance, ===, ---, ///, =-=, etc.  Try to omit these
type of content by checking the word boundary's existence."
  (string-match-p "\\w" content))

(defun ts-fold-summary--apply-sym (line sym)
  "Remove SYM from LINE."
  (when (string-prefix-p sym line)
    (setq line (substring line (length sym) (length line))
          line (string-trim line)))
  line)

(defun ts-fold-summary--extract-summary (doc-str sym)
  "Extract only document summary from DOC-STR using SYM."
  (let ((lines (split-string doc-str "\n")) new-lines)
    (dolist (line lines)
      (setq line (string-trim line))
      (cond ((listp sym)
             (dolist (c sym) (setq line (ts-fold-summary--apply-sym line c))))
            (t (setq line (ts-fold-summary--apply-sym line sym))))
      (when (ts-fold-summary--valid-content-p line) (push line new-lines)))
    (reverse new-lines)))

(defun ts-fold-summary--doc-extract (doc-str sym)
  "Default way to extract the doc summary from DOC-STR using SYM."
  (let* ((lines (ts-fold-summary--extract-summary doc-str sym)) (summary (nth 0 lines)))
    (when summary (setq summary (string-trim summary)))
    (if (string-empty-p summary) nil summary)))

(defun ts-fold-summary--generic (doc-str sym)
  "Generic DOC-STR extraction using SYM."
  (when (ts-fold--doc-faces-p doc-str)
    (ts-fold-summary--doc-extract doc-str sym)))

(defun ts-fold-summary-batch (doc-str)
  "Extract summary from DOC-STR in Batch."
  (ts-fold-summary--generic doc-str '("::" "rem" "REM")))

(defun ts-fold-summary-csharp-vsdoc (doc-str)
  "Extract summary from DOC-STR in C# vsdoc."
  (let ((type-triple (string-match-p "///" doc-str)))
    (setq doc-str (s-replace-regexp "<[/]*[^>]+." "" doc-str))
    (ts-fold-summary--generic doc-str (if type-triple "///" "//"))))

(defun ts-fold-summary-csharp (doc-str)
  "Extract summary from DOC-STR in C#."
  (cond ((string-match-p "///" doc-str)
         (ts-fold-summary-csharp-vsdoc doc-str))
        (t (ts-fold-summary-javadoc doc-str))))

(defun ts-fold-summary-elisp (doc-str)
  "Extract summary from DOC-STR in Elisp."
  (ts-fold-summary--generic doc-str ";;"))

(defun ts-fold-summary-javadoc (doc-str)
  "Extract summary from DOC-STR in Javadoc."
  (ts-fold-summary--generic doc-str "*"))

(defun ts-fold-summary-go (doc-str)
  "Extract summary from DOC-STR in Go."
  (ts-fold-summary--generic doc-str "//"))

(defun ts-fold-summary-lua-doc (doc-str)
  "Extract summary from DOC-STR in Lua."
  (ts-fold-summary--generic doc-str "--"))

(defun ts-fold-summary-pascal-doc (doc-str)
  "Extract summary from DOC-STR in Pascal."
  (cond ((string-prefix-p "{" doc-str)
         (ts-fold-summary--generic doc-str '("{" "}")))
        (t (ts-fold-summary-go doc-str))))

(defun ts-fold-summary-python-doc (doc-str)
  "Extract summary from DOC-STR in Python."
  (ts-fold-summary--generic doc-str "\"\"\""))

(defun ts-fold-summary-rst-doc (doc-str)
  "Extract summary from DOC-STR in reStructuredText."
  (ts-fold-summary--generic doc-str ".."))

(defun ts-fold-summary-ruby-doc (doc-str)
  "Extract summary from DOC-STR in Ruby."
  (ts-fold-summary--generic doc-str "#"))

(defun ts-fold-summary-rust-doc (doc-str)
  "Extract summary from DOC-STR in Rust."
  (ts-fold-summary--generic doc-str "///"))

(defun ts-fold-summary-tex-doc (doc-str)
  "Extract summary from DOC-STR in Tex family."
  (ts-fold-summary--generic doc-str "%"))

(defun ts-fold-summary-c-macro (doc-str)
  "Parse C macro summary from DOC-STR."
  (when (ts-fold--is-face doc-str
                          '(font-lock-preprocessor-face
                            preproc-font-lock-preprocessor-background))
    (ts-fold-summary--doc-extract doc-str "")))

(defun ts-fold-summary-c (doc-str)
  "Extract summary from DOC-STR in C comment."
  (or (ts-fold-summary-javadoc doc-str)
      (ts-fold-summary-c-macro doc-str)))

(defun ts-fold-summary-markdown (doc-str)
  "Extract summary from DOC-STR in Markdown block."
  (ts-fold-summary--doc-extract doc-str '("-" "```")))

(defun ts-fold-summary-org (doc-str)
  "Extract summary from DOC-STR in Org block."
  (ts-fold-summary--doc-extract doc-str '()))

(defun ts-fold-summary-xml (doc-str)
  "Extract summary from DOC-STR in XML."
  (ts-fold-summary--generic doc-str "-"))

(defun ts-fold-summary-julia-doc (doc-str)
  "Extract summary from DOC-STR in julia."
  (ts-fold-summary--generic doc-str "\"\"\""))

;;
;; (@* "Core" )
;;

(defun ts-fold-summary--keep-length (summary)
  "Keep the SUMMARY length to `ts-fold-summary-max-length'."
  (let ((len-sum (length summary))
        (len-exc (length ts-fold-summary-exceeded-string)))
    (when (< ts-fold-summary-max-length len-sum)
      (setq summary (substring summary 0 (- ts-fold-summary-max-length len-exc))
            summary (concat summary ts-fold-summary-exceeded-string))))
  summary)

(defun ts-fold-summary--apply-format (summary)
  "Return the SUMMARY that has added the summary prefix."
  (format ts-fold-summary-format summary))

(defun ts-fold-summary--parser ()
  "Return the summary parser from `ts-fold-summary-parsers-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) ts-fold-summary-parsers-alist))

(defun ts-fold-summary--get (doc-str)
  "Extract summary from DOC-STR in order to display ontop of the overlay."
  (let ((parser (cdr (ts-fold-summary--parser))) summary)
    (when parser
      (setq summary (funcall parser doc-str))
      (when (integerp ts-fold-summary-max-length)
        (setq summary (ts-fold-summary--keep-length summary)))
      (when summary
        (setq summary (ts-fold-summary--apply-format summary)
              summary (propertize summary 'face 'ts-fold-replacement-face))))
    summary))

;; TODO(everyone): keep this alist alphabetically sorted
(defcustom ts-fold-summary-parsers-alist
  `((actionscript-mode      . ts-fold-summary-javadoc)
    (fasm-mode              . ts-fold-summary-elisp)
    (masm-mode              . ts-fold-summary-elisp)
    (nasm-mode              . ts-fold-summary-elisp)
    (bat-mode               . ts-fold-summary-batch)
    (beancount-mode         . ts-fold-summary-elisp)
    (c-mode                 . ts-fold-summary-c)
    (c++-mode               . ts-fold-summary-c)
    (cmake-mode             . ts-fold-summary-ruby-doc)
    (clojure-mode           . ts-fold-summary-elisp)
    (csharp-mode            . ts-fold-summary-csharp)
    (css-mode               . ts-fold-summary-javadoc)
    (dart-mode              . ts-fold-summary-javadoc)
    (emacs-lisp-mode        . ts-fold-summary-elisp)
    (elixir-mode            . ts-fold-summary-ruby-doc)
    (erlang-mode            . ts-fold-summary-tex-doc)
    (gdscript-mode          . ts-fold-summary-ruby-doc)
    (go-mode                . ts-fold-summary-go)
    (haskell-mode           . ts-fold-summary-lua-doc)
    (html-mode              . ts-fold-summary-xml)
    (jai-mode               . ts-fold-summary-c)
    (java-mode              . ts-fold-summary-javadoc)
    (javascript-mode        . ts-fold-summary-javadoc)
    (js-mode                . ts-fold-summary-javadoc)
    (js2-mode               . ts-fold-summary-javadoc)
    (js3-mode               . ts-fold-summary-javadoc)
    (jsonnet-mode           . ts-fold-summary-javadoc)
    (julia-mode             . ts-fold-summary-julia-doc)
    (kotlin-mode            . ts-fold-summary-javadoc)
    (latex-mode             . ts-fold-summary-tex-doc)
    (lua-mode               . ts-fold-summary-lua-doc)
    (makefile-mode          . ts-fold-summary-ruby-doc)
    (makefile-automake-mode . ts-fold-summary-ruby-doc)
    (makefile-gmake-mode    . ts-fold-summary-ruby-doc)
    (makefile-makepp-mode   . ts-fold-summary-ruby-doc)
    (makefile-bsdmake-mode  . ts-fold-summary-ruby-doc)
    (makefile-imake-mode    . ts-fold-summary-ruby-doc)
    (markdown-mode          . ts-fold-summary-markdown)
    (nix-mode               . ts-fold-summary-ruby-doc)
    (noir-mode              . ts-fold-summary-rust-doc)
    (objc-mode              . ts-fold-summary-c)
    (org-mode               . ts-fold-summary-org)
    (perl-mode              . ts-fold-summary-ruby-doc)
    (php-mode               . ts-fold-summary-javadoc)
    (pascal-mode            . ts-fold-summary-pascal-doc)
    (python-mode            . ts-fold-summary-python-doc)
    (rjsx-mode              . ts-fold-summary-javadoc)
    (rst-mode               . ts-fold-summary-rst-doc)
    (ruby-mode              . ts-fold-summary-ruby-doc)
    (rust-mode              . ts-fold-summary-rust-doc)
    (scala-mode             . ts-fold-summary-javadoc)
    (scheme-mode            . ts-fold-summary-elisp)
    (sh-mode                . ts-fold-summary-javadoc)
    (sql-mode               . ts-fold-summary-c)
    (swift-mode             . ts-fold-summary-c)
    (toml-mode              . ts-fold-summary-javadoc)
    (conf-toml-mode         . ts-fold-summary-javadoc)
    (typescript-mode        . ts-fold-summary-javadoc)
    (verilog-mode           . ts-fold-summary-javadoc)
    (vhdl-mode              . ts-fold-summary-lua-doc)
    (nxml-mode              . ts-fold-summary-xml)
    (yaml-mode              . ts-fold-summary-ruby-doc)
    (zig-mode               . ts-fold-summary-go))
  "Alist mapping `major-mode' to doc parser function."
  :type '(alist :key-type symbol :value-type function)
  :group 'ts-fold)

(provide 'ts-fold-summary)
;;; ts-fold-summary.el ends here
