;;; ts-fold-parsers.el --- Adapter layer to Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Shen, Jen-Chieh
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

;; TODO(everyone): keep the forward declared alphabetically sorted

(declare-function ts-fold-range-seq "ts-fold.el")
(declare-function ts-fold-range-line-comment "ts-fold.el")
(declare-function ts-fold-range-block-comment "ts-fold.el")
(declare-function ts-fold-range-c-like-comment "ts-fold.el")

(declare-function ts-fold-range-c-preproc-ifdef "ts-fold.el")
(declare-function ts-fold-range-c-preproc-if "ts-fold.el")
(declare-function ts-fold-range-c-preproc-elif "ts-fold.el")
(declare-function ts-fold-range-c-preproc-else "ts-fold.el")
(declare-function ts-fold-range-html "ts-fold.el")
(declare-function ts-fold-range-julia "ts-fold.el")
(declare-function ts-fold-range-ocaml "ts-fold.el")
(declare-function ts-fold-range-python "ts-fold.el")
(declare-function ts-fold-range-ruby-class-def "ts-fold.el")
(declare-function ts-fold-range-ruby-if "ts-fold.el")
(declare-function ts-fold-range-rust-macro "ts-fold.el")
(declare-function ts-fold-range-elixir "ts-fold.el")

;;
;; (@* "Parsers" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun ts-fold-parsers-agda ()
  "Rule set for Agda."
  '(()))

(defun ts-fold-parsers-bash ()
  "Rule set for Bash."
  '((compound_statement . ts-fold-range-seq)
    (do_group           . (ts-fold-range-seq 1 -3))
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-c ()
  "Rule set for C."
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
  "Rule set for C++."
  (append (ts-fold-parsers-c)))

(defun ts-fold-parsers-csharp ()
  "Rule set for C#."
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
  "Rule set for CSS."
  '((keyframe_block_list . ts-fold-range-seq)
    (block               . ts-fold-range-seq)
    (comment             . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-elixir ()
  "Rules set for Elixir."
  '((list . ts-fold-range-seq)
    (map . ts-fold-range-seq)
    (tuple . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))
    (do_block . ts-fold-range-elixir)))

(defun ts-fold-parsers-go ()
  "Rule set for Go."
  '((block                  . ts-fold-range-seq)
    (comment                . ts-fold-range-seq)
    (const_declaration      . (lambda (node offset)
                                (ts-fold-range-markers node offset "(" ")")))
    (field_declaration_list . ts-fold-range-seq)
    (import_spec_list       . ts-fold-range-seq)
    (interface_type         . (lambda (node offset)
                                (ts-fold-range-markers node offset "{" "}")))))

(defun ts-fold-parsers-html ()
  "Rule set for HTML."
  '((element . ts-fold-range-html)
    (comment . (ts-fold-range-seq 1 -1))))

(defun ts-fold-parsers-java ()
  "Rule set for Java."
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
    (block_comment                   . (ts-fold-range-seq 1 -1))))

(defun ts-fold-parsers-javascript ()
  "Rule set for JavaScript."
  '((export_clause   . ts-fold-range-seq)
    (statement_block . ts-fold-range-seq)
    (object          . ts-fold-range-seq)
    (array           . ts-fold-range-seq)
    (comment         . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-json ()
  "Rule set for JSON."
  '((object . ts-fold-range-seq)
    (array  . ts-fold-range-seq)))

(defun ts-fold-parsers-julia ()
  "Rule set for Julia."
  '((block_comment       . (ts-fold-range-seq 1 -1))
    (for_statement       . (ts-fold-range-seq 2 -2))
    (function_definition . ts-fold-range-julia)
    (if_statement        . (ts-fold-range-seq 1 -2))
    (let_statement       . (ts-fold-range-seq 2 -2))
    (macro_definition    . ts-fold-range-julia)
    (module_definition   . ts-fold-range-julia)
    (quote_statement     . ts-fold-range-julia)
    (struct_definition   . ts-fold-range-julia)
    (triple_string       . (ts-fold-range-seq 2 -2))
    (try_statement       . (ts-fold-range-seq 2 -2))
    (while_statement     . ts-fold-range-julia)))

(defun ts-fold-parsers-nix ()
  "Rule set for Nix."
  '((attrset       . ts-fold-range-seq)
    (interpolation . ts-fold-range-seq)
    (list          . ts-fold-range-seq)))

(defun ts-fold-parsers-ocaml ()
  "Rule set for OCaml."
  '((comment             . ts-fold-range-ocaml-comment)
    (module_definition   . ts-fold-range-ocaml-module-definition)
    (type_definition     . ts-fold-range-ocaml-type-definition)
    (value_definition    . ts-fold-range-ocaml-value-definition)))

(defun ts-fold-parsers-php ()
  "Rule set for PHP."
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
  "Rule set for Python."
  '((function_definition . ts-fold-range-python)
    (class_definition    . ts-fold-range-python)
    (list                . ts-fold-range-seq)
    (dictionary          . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-r ()
  "Rule set for R."
  '((brace_list . ts-fold-range-seq)))

(defun ts-fold-parsers-ruby ()
  "Rule set for Ruby."
  '((class    . ts-fold-range-ruby-class-def)
    (method   . ts-fold-range-ruby-class-def)
    (array    . ts-fold-range-seq)
    (do       . (ts-fold-range-seq 1 -2))  ; match with `end`
    (do_block . (ts-fold-range-seq 1 -2))  ; match with `end`, in spec file
    (then     . ts-fold-range-ruby-if)        ; `if` and `elsif` block
    (else     . (ts-fold-range-ruby-if 4 0))  ; `else` block
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-rust ()
  "Rule set for Rust."
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
  "Rule set for Scala."
  '((import_selectors . ts-fold-range-seq)
    (template_body    . ts-fold-range-seq)
    (block            . ts-fold-range-seq)
    (comment          . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-swift ()
  "Rule set for Swift."
  '((switch_statement      . ts-fold-range-seq)
    (function_declaration  . ts-fold-range-seq)
    (enum_declaration      . ts-fold-range-seq)
    (struct_declaration    . ts-fold-range-seq)
    (class_declaration     . ts-fold-range-seq)
    (protocol_declaration  . ts-fold-range-seq)
    (extension_declaration . ts-fold-range-seq)
    (comment               . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-typescript ()
  "Rule set for TypeScript."
  (append (ts-fold-parsers-javascript)))

(defun ts-fold-parsers-yaml ()
  "Rule set for YAML."
  '((comment . (lambda (node offset) (ts-fold-range-line-comment node offset "#")))
    (block_mapping_pair . ((lambda (node offset) (ts-fold-range-markers node offset ":")) 0 1))))

(provide 'ts-fold-parsers)
;;; ts-fold-parsers.el ends here
