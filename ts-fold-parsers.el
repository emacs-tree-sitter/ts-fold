;;; ts-fold-parsers.el --- Adapter layer to Tree-Sitter  -*- lexical-binding: t; -*-

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
;; the rule sets!
;;

;;; Code:

;;
;; (@* "Externals" )
;;

(declare-function ts-fold-range-seq "ts-fold.el")
(declare-function ts-fold-range-line-comment "ts-fold.el")
(declare-function ts-fold-range-block-comment "ts-fold.el")
(declare-function ts-fold-range-c-like-comment "ts-fold.el")

(declare-function ts-fold-range-c-preproc-ifdef "ts-fold.el")
(declare-function ts-fold-range-c-preproc-if "ts-fold.el")
(declare-function ts-fold-range-c-preproc-elif "ts-fold.el")
(declare-function ts-fold-range-c-preproc-else "ts-fold.el")
(declare-function ts-fold-range-html "ts-fold.el")
(declare-function ts-fold-range-python "ts-fold.el")
(declare-function ts-fold-range-ruby "ts-fold.el")
(declare-function ts-fold-range-rust-macro "ts-fold.el")

;;
;; (@* "Parsers" )
;;

(defun ts-fold-parsers-agda ()
  "Rule sets for Agda."
  '(()))

(defun ts-fold-parsers-bash ()
  "Rule sets for Bash."
  '((compound_statement . ts-fold-range-seq)
    (expansion          . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-c ()
  "Rule sets for C."
  '((compound_statement     . ts-fold-range-seq)
    (declaration_list       . ts-fold-range-seq)
    (enumerator_list        . ts-fold-range-seq)
    (field_declaration_list . ts-fold-range-seq)
    (preproc_if             . ts-fold-range-c-preproc-if)
    (preproc_ifdef          . ts-fold-range-c-preproc-ifdef)
    (preproc_elif           . ts-fold-range-c-preproc-elif)
    (preproc_else           . ts-fold-range-c-preproc-else)
    (comment                . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-c++ ()
  "Rule sets for C++."
  (append (ts-fold-parsers-c)))

(defun ts-fold-parsers-csharp ()
  "Rule sets for C#."
  '((block                                . ts-fold-range-seq)
    (accessor_list                        . ts-fold-range-seq)
    (enum_member_declaration_list         . ts-fold-range-seq)
    (declaration_list                     . ts-fold-range-seq)
    (switch_body                          . ts-fold-range-seq)
    (anonymous_object_creation_expression . ts-fold-range-seq)
    (initializer_expression               . ts-fold-range-seq)
    ;;(if_directive                         . ts-fold-range-seq)
    ;;(else_directive                       . ts-fold-range-seq)
    ;;(elif_directive                       . ts-fold-range-seq)
    ;;(endif_directive                      . ts-fold-range-seq)
    ;;(region_directive                     . ts-fold-range-seq)
    ;;(endregion_directive                  . ts-fold-range-seq)
    (comment                              . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-css ()
  "Rule sets for CSS."
  '((keyframe_block_list . ts-fold-range-seq)
    (block               . ts-fold-range-seq)
    (comment             . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-go ()
  "Rule sets for Go."
  '((block   . ts-fold-range-seq)
    (comment . ts-fold-range-seq)))

(defun ts-fold-parsers-html ()
  "Rule sets for HTML."
  '((element . ts-fold-range-html)
    (comment . (ts-fold-range-seq 1 -1))))

(defun ts-fold-parsers-java ()
  "Rule sets for Java."
  '((switch_block                    . ts-fold-range-seq)
    (block                           . ts-fold-range-seq)
    (element_value_array_initializer . ts-fold-range-seq)
    (module_body                     . ts-fold-range-seq)
    (enum_body                       . ts-fold-range-seq)
    (class_body                      . ts-fold-range-seq)
    (constructor_body                . ts-fold-range-seq)
    (annotation_type_body            . ts-fold-range-seq)
    (interface_body                  . ts-fold-range-seq)
    (array_initializer               . ts-fold-range-seq)
    (comment                         . (ts-fold-range-seq 1 -1))))

(defun ts-fold-parsers-javascript ()
  "Rule sets for JavaScript."
  '((export_clause   . ts-fold-range-seq)
    (statement_block . ts-fold-range-seq)
    (comment         . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-json ()
  "Rule sets for JSON."
  '((object . ts-fold-range-seq)
    (array  . ts-fold-range-seq)))

(defun ts-fold-parsers-nix ()
  "Rule sets for Nix."
  '((attrset       . ts-fold-range-seq)
    (interpolation . ts-fold-range-seq)
    (list          . ts-fold-range-seq)))

(defun ts-fold-parsers-php ()
  "Rule sets for PHP."
  '((namespace_use_group . ts-fold-range-seq)
    (declaration_list    . ts-fold-range-seq)
    (use_list            . ts-fold-range-seq)
    (switch_block        . ts-fold-range-seq)
    (compound_statement  . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (if (string-prefix-p "#" (tsc-node-text node))
             (ts-fold-range-line-comment node offset "#")
           (ts-fold-range-c-like-comment node offset))))))

(defun ts-fold-parsers-python ()
  "Rule sets for Python."
  '((function_definition . ts-fold-range-python)
    (class_definition    . ts-fold-range-python)
    (list                . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-r ()
  "Rule sets for R."
  '((brace_list . ts-fold-range-seq)))

(defun ts-fold-parsers-ruby ()
  "Rule sets for Ruby."
  '((class  . ts-fold-range-ruby)
    (method . ts-fold-range-ruby)
    (array  . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-rust ()
  "Rule sets for Rust."
  '((declaration_list       . ts-fold-range-seq)
    (enum_variant_list      . ts-fold-range-seq)
    (field_declaration_list . ts-fold-range-seq)
    (use_list               . ts-fold-range-seq)
    (field_initializer_list . ts-fold-range-seq)
    (match_block            . ts-fold-range-seq)
    (macro_definition       . (ts-fold-range-rust-macro 1 -1))
    (block                  . ts-fold-range-seq)
    (line_comment           . (lambda (node offset)
                                (ts-fold-range-line-comment node offset "///")))
    (block_comment          . ts-fold-range-block-comment)))

(defun ts-fold-parsers-scala ()
  "Rule sets for Scala."
  '((import_selectors . ts-fold-range-seq)
    (template_body    . ts-fold-range-seq)
    (block            . ts-fold-range-seq)
    (comment          . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-swift ()
  "Rule sets for Swift."
  '((switch_statement      . ts-fold-range-seq)
    (function_declaration  . ts-fold-range-seq)
    (enum_declaration      . ts-fold-range-seq)
    (struct_declaration    . ts-fold-range-seq)
    (class_declaration     . ts-fold-range-seq)
    (protocol_declaration  . ts-fold-range-seq)
    (extension_declaration . ts-fold-range-seq)
    (comment               . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-typescript ()
  "Rule sets for TypeScript."
  (append (ts-fold-parsers-javascript)))

(provide 'ts-fold-parsers)
;;; ts-fold-parsers.el ends here
