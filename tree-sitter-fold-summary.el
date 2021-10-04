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

(defcustom tree-sitter-fold-show-summary t
  "Flag to show summary if available."
  :type 'boolean
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-max-summary-length 60
  "Maximum length for summary to display."
  :type '(choice (const :tag "nil" nil)
                 (integer :tag "positive integer number"))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-summary-exceeded-string "..."
  "String that added after display summary.
This happens only when summary length is larger than variable
`tree-sitter-fold-max-summary-length'."
  :type 'string
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-summary-format " <S> %s "
  "Prefix string added before summary overlay."
  :type 'string
  :group 'tree-sitter-fold)

(defun tree-sitter-fold--keep-summary-length (summary)
  "Keep the SUMMARY length to `tree-sitter-fold-max-summary-length'."
  (let ((len-sum (length summary))
        (len-exc (length tree-sitter-fold-summary-exceeded-string)))
    (when (< tree-sitter-fold-max-summary-length len-sum)
      (setq summary (substring summary 0 (- tree-sitter-fold-max-summary-length len-exc))
            summary (concat summary tree-sitter-fold-summary-exceeded-string))))
  summary)

(defun tree-sitter-fold--get-summary (doc-str)
  "Extract summary from DOC-STR in order to display ontop of the overlay."
  (let ((parser (cdr (origami-get-summary-parser))) summary)
    (when parser
      (setq summary (funcall parser doc-str))
      (when (integerp tree-sitter-fold-max-summary-length)
        (setq summary (tree-sitter-fold--keep-summary-length summary)))
      (when summary
        (setq summary (origami-summary-apply-format summary)
              summary (propertize summary 'face 'tree-sitter-fold-replacement-face))))
    summary))

(provide 'tree-sitter-fold-summary)
;;; tree-sitter-fold-summary.el ends here
