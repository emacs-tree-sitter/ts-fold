;;; tree-sitter-fold-parsers.el --- Adapter layer to Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-10-04 17:45:48

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
;; Adapter layer to Tree-Sitter
;;
;; This isn't a real parser implementation, but records down the rule
;; in order to let the Tree-Sitter to parse things correctly.  Think of
;; rule sets!
;;

;;; Code:

(defun tree-sitter-fold-parsers-c ()
  ""
  '((compound_statement     . tree-sitter-fold-range-seq)
    (declaration_list       . tree-sitter-fold-range-seq)
    (enumerator_list        . tree-sitter-fold-range-seq)
    (field_declaration_list . tree-sitter-fold-range-seq)
    (comment                . (tree-sitter-fold-range-seq 1 -1))))

(defun tree-sitter-fold-parsers-c++ ()
  ""
  (append (tree-sitter-fold-parsers-c)))

(provide 'tree-sitter-fold-parsers)
;;; tree-sitter-fold-parsers.el ends here
