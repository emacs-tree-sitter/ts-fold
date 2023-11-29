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

(declare-function ts-fold-range-asm-label "ts-fold.el")
(declare-function ts-fold-range-beancount-transaction "ts-fold.el")
(declare-function ts-fold-range-c-preproc-ifdef "ts-fold.el")
(declare-function ts-fold-range-c-preproc-if "ts-fold.el")
(declare-function ts-fold-range-c-preproc-elif "ts-fold.el")
(declare-function ts-fold-range-c-preproc-else "ts-fold.el")
(declare-function ts-fold-range-elisp-function "ts-fold.el")
(declare-function ts-fold-range-elixir "ts-fold.el")
(declare-function ts-fold-range-erlang-clause-body "ts-fold.el")
(declare-function ts-fold-range-erlang-type-guards "ts-fold.el")
(declare-function ts-fold-range-fish-function "ts-fold.el")
(declare-function ts-fold-range-fish-if "ts-fold.el")
(declare-function ts-fold-range-fish-case "ts-fold.el")
(declare-function ts-fold-range-haskell-function "ts-fold.el")
(declare-function ts-fold-range-html "ts-fold.el")
(declare-function ts-fold-range-julia "ts-fold.el")
(declare-function ts-fold-range-kotlin-when "ts-fold.el")
(declare-function ts-fold-range-lisp-function "ts-fold.el")
(declare-function ts-fold-range-lua-comment "ts-fold.el")
(declare-function ts-fold-range-lua-function "ts-fold.el")
(declare-function ts-fold-range-lua-if "ts-fold.el")
(declare-function ts-fold-range-lua-elseif "ts-fold.el")
(declare-function ts-fold-range-lua-else "ts-fold.el")
(declare-function ts-fold-range-lua-do-loop "ts-fold.el")
(declare-function ts-fold-range-lua-repeat "ts-fold.el")
(declare-function ts-fold-range-make-recipe "ts-fold.el")
(declare-function ts-fold-range-mermaid-diagram "ts-fold.el")
(declare-function ts-fold-range-mermaid-block "ts-fold.el")
(declare-function ts-fold-range-ocaml-comment "ts-fold.el")
(declare-function ts-fold-range-ocaml-module-definition "ts-fold.el")
(declare-function ts-fold-range-ocaml-type-definition "ts-fold.el")
(declare-function ts-fold-range-ocaml-value-definition "ts-fold.el")
(declare-function ts-fold-range-org-body "ts-fold.el")
(declare-function ts-fold-range-clojure-function "ts-fold.el")
(declare-function ts-fold-range-pascal-comment "ts-fold.el")
(declare-function ts-fold-range-python-def "ts-fold.el")
(declare-function ts-fold-range-python-expression-statement "ts-fold.el")
(declare-function ts-fold-range-rst-body "ts-fold.el")
(declare-function ts-fold-range-ruby-class-def "ts-fold.el")
(declare-function ts-fold-range-ruby-if "ts-fold.el")
(declare-function ts-fold-range-rust-macro "ts-fold.el")
(declare-function ts-fold-range-sql-block "ts-fold.el")
(declare-function ts-fold-range-toml-table "ts-fold.el")
(declare-function ts-fold-range-verilog-list "ts-fold.el")
(declare-function ts-fold-range-verilog-initial-construct "ts-fold.el")
(declare-function ts-fold-range-verilog-module "ts-fold.el")
(declare-function ts-fold-range-vhdl-package "ts-fold.el")
(declare-function ts-fold-range-vhdl-type "ts-fold.el")

;;
;; (@* "Parsers" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun ts-fold-parsers-agda ()
  "Rule set for Agda."
  '(()))

(defun ts-fold-parsers-arduino ()
  "Rule set for Arduino."
  (append (ts-fold-parsers-c++)))

(defun ts-fold-parsers-asm ()
  "Rule set for Assembly."
  '((label . ts-fold-range-asm-label)
    (line_comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset ";;")))))

(defun ts-fold-parsers-bash ()
  "Rule set for Bash."
  '((compound_statement . ts-fold-range-seq)
    (do_group           . (ts-fold-range-seq 1 -3))
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-beancount ()
  "Rule set for Beancount."
  '((transaction . ts-fold-range-beancount-transaction)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset ";;")))))

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

(defun ts-fold-parsers-clojure ()
  "Rule set for Clojure."
  '((list_lit . ts-fold-range-clojure-function)
    (map_lit  . ts-fold-range-seq)
    (str_lit  . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset ";;")))))

(defun ts-fold-parsers-cmake ()
  "Rule set for CMake."
  '((body . ts-fold-range-seq)
    (line_comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

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

(defun ts-fold-parsers-dart ()
  "Rule set for Dart."
  '((block                 . ts-fold-range-seq)
    (class_body            . ts-fold-range-seq)
    (arguments             . ts-fold-range-seq)
    (comment               . ts-fold-range-c-like-comment)
    (documentation_comment . ts-fold-range-c-like-comment)
    (list_literal          . ts-fold-range-seq)))  ; array

(defun ts-fold-parsers-elisp ()
  "Rule set for Elisp."
  '((macro_definition    . ts-fold-range-elisp-function)
    (function_definition . ts-fold-range-elisp-function)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset ";;")))))

(defun ts-fold-parsers-elixir ()
  "Rules set for Elixir."
  '((list     . ts-fold-range-seq)
    (map      . ts-fold-range-seq)
    (tuple    . ts-fold-range-seq)
    (do_block . ts-fold-range-elixir)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-erlang ()
  "Rules set for Erlang."
  '((list        . ts-fold-range-seq)
    (clause_body . ts-fold-range-erlang-clause-body)
    (type_guards . ts-fold-range-erlang-type-guards)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "%")))))

(defun ts-fold-parsers-fish ()
  "Rules set for Fish."
  '((function_definition . ts-fold-range-fish-function)
    (if_statement        . ts-fold-range-fish-if)
    (switch_statement    . ts-fold-range-fish-if)
    (for_statement       . ts-fold-range-fish-if)
    (while_statement     . ts-fold-range-fish-if)
    (case_clause         . ts-fold-range-fish-case)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-gdscript ()
  "Rule set for GGScript."
  '((body . (ts-fold-range-seq -1 1))
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-glsl ()
  "Rule set for GLSL."
  '((field_declaration_list . ts-fold-range-seq)
    (compound_statement     . ts-fold-range-seq)
    (comment                . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-go ()
  "Rule set for Go."
  '((block                  . ts-fold-range-seq)
    (comment                . ts-fold-range-c-like-comment)
    (const_declaration      . (lambda (node offset)
                                (ts-fold-range-markers node offset "(" ")")))
    (field_declaration_list . ts-fold-range-seq)
    (import_spec_list       . ts-fold-range-seq)
    (interface_type         . (lambda (node offset)
                                (ts-fold-range-markers node offset "{" "}")))))

(defun ts-fold-parsers-groovy ()
  "Rule set for Groovy."
  '((block         . ts-fold-range-groovy-block)
    (line_comment  . ts-fold-range-c-like-comment)
    (block_comment . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-haskell ()
  "Rule set for Haskell."
  '((function . ts-fold-range-haskell-function)
    (comment  . ts-fold-range-lua-comment)))

(defun ts-fold-parsers-hlsl ()
  "Rule set for HLSL."
  '((field_declaration_list . ts-fold-range-seq)
    (compound_statement     . ts-fold-range-seq)
    (comment                . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-html ()
  "Rule set for HTML."
  '((element . ts-fold-range-html)
    (comment . (ts-fold-range-seq 1 -1))))

(defun ts-fold-parsers-jai ()
  "Rule set for Jai."
  '((imperative_scope . ts-fold-range-seq)
    (data_scope       . ts-fold-range-seq)
    (block_comment    . ts-fold-range-block-comment)
    (inline_comment   . ts-fold-range-c-like-comment)))

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
    (block_comment                   . ts-fold-range-block-comment)
    (line_comment                    . ts-fold-range-c-like-comment)))

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

(defun ts-fold-parsers-jsonnet ()
  "Rule set for Jsonnet."
  '((object  . ts-fold-range-seq)
    (array   . ts-fold-range-seq)
    (comment . ts-fold-range-c-like-comment)))

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

(defun ts-fold-parsers-kotlin ()
  "Rule set for Kotlin."
  '((function_body          . ts-fold-range-seq)
    (control_structure_body . ts-fold-range-seq)
    (lambda_literal         . ts-fold-range-seq)
    (enum_class_body        . ts-fold-range-seq)
    (class_body             . ts-fold-range-seq)
    (when_expression        . ts-fold-range-kotlin-when)
    (multiline_comment      . ts-fold-range-c-like-comment)
    (line_comment           . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-latex ()
  "Rule set for LaTex."
  '((curly_group . ts-fold-range-seq)
    (line_comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "%")))))

(defun ts-fold-parsers-lisp ()
  "Rule set for Lisp."
  '((defun . ts-fold-range-lisp-function)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node
                                     (ts-fold--cons-add offset '(0 . -1))
                                     ";;")))))

(defun ts-fold-parsers-lua ()
  "Rule set for Lua."
  '((expression_list      . ts-fold-range-seq)
    (function_declaration . ts-fold-range-lua-function)
    (if_statement         . ts-fold-range-lua-if)
    (elseif_statement     . ts-fold-range-lua-elseif)
    (else_statement       . ts-fold-range-lua-else)
    (while_statement      . ts-fold-range-lua-do-loop)
    (for_statement        . ts-fold-range-lua-do-loop)
    (repeat_statement     . ts-fold-range-lua-repeat)
    (comment              . ts-fold-range-lua-comment)))

(defun ts-fold-parsers-make ()
  "Rule set for Make."
  '((recipe . ts-fold-range-make-recipe)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-markdown ()
  "Rule set for Markdown."
  '((fenced_code_block . (ts-fold-range-seq 2 -2))
    (html_block        . ts-fold-range-html)))

(defun ts-fold-parsers-mermaid ()
  "Rule set for Mermaid."
  '((diagram_flow         . ts-fold-range-mermaid-diagram)
    (diagram_sequence     . ts-fold-range-mermaid-diagram)
    (diagram_class        . ts-fold-range-mermaid-diagram)
    (diagram_er           . ts-fold-range-mermaid-diagram)
    (class_stmt_class     . ts-fold-range-mermaid-block)
    (er_stmt_entity_block . ts-fold-range-mermaid-block)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node
                                     (ts-fold--cons-add offset '(0 . -1))
                                     "%%")))))

(defun ts-fold-parsers-noir ()
  "Rule set for Noir."
  '((body    . ts-fold-range-seq)
    (comment . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-nix ()
  "Rule set for Nix."
  '((attrset_expression . ts-fold-range-seq)
    (interpolation      . ts-fold-range-seq)
    (list_expression    . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-ocaml ()
  "Rule set for OCaml."
  '((comment             . ts-fold-range-ocaml-comment)
    (module_definition   . ts-fold-range-ocaml-module-definition)
    (type_definition     . ts-fold-range-ocaml-type-definition)
    (value_definition    . ts-fold-range-ocaml-value-definition)))

(defun ts-fold-parsers-org ()
  "Rule set for Org."
  '((body    . ts-fold-range-org-body)
    (block   . ts-fold-range-seq)
    (comment . ts-fold-range-seq)))

(defun ts-fold-parsers-pascal ()
  "Rule set for Pascal."
  '((comment . ts-fold-range-pascal-comment)))

(defun ts-fold-parsers-perl ()
  "Rule set for Perl."
  '((block           . ts-fold-range-seq)
    (list_expression . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

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
  '((function_definition  . ts-fold-range-python-def)
    (class_definition     . ts-fold-range-python-def)
    (list                 . ts-fold-range-seq)
    (dictionary           . ts-fold-range-seq)
    (expression_statement . ts-fold-range-python-expression-statement)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-r ()
  "Rule set for R."
  '((brace_list . ts-fold-range-seq)))

(defun ts-fold-parsers-rst ()
  "Rule set for reStructuredText."
  '((body    . ts-fold-range-rst-body)
    (comment . (ts-fold-range-seq 1 0))))

(defun ts-fold-parsers-ruby ()
  "Rule set for Ruby."
  '((class    . ts-fold-range-ruby-class-def)
    (method   . ts-fold-range-ruby-class-def)
    (array    . ts-fold-range-seq)
    (do       . (ts-fold-range-seq 1 -2))     ; match with `end`
    (do_block . (ts-fold-range-seq 1 -2))     ; match with `end`, in spec file
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

(defun ts-fold-parsers-scheme ()
  "Rule set for Scheme."
  '((list . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset ";;")))))

(defun ts-fold-parsers-sql ()
  "Rule set for SQL."
  '((block      . ts-fold-range-sql-block)
    (subquery   . ts-fold-range-seq)
    (list       . ts-fold-range-seq)
    (marginalia . ts-fold-range-c-like-comment)))  ; This is the comment!

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

(defun ts-fold-parsers-toml ()
  "Rule set for TOML."
  '((table . ts-fold-range-toml-table)
    (array . ts-fold-range-seq)
    (comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))))

(defun ts-fold-parsers-typescript ()
  "Rule set for TypeScript."
  (append
   (ts-fold-parsers-javascript)
   '((class_body    . ts-fold-range-seq)
     (enum_body     . ts-fold-range-seq)
     (named_imports . ts-fold-range-seq)
     (object_type   . ts-fold-range-seq))))

(defun ts-fold-parsers-verilog ()
  "Rule set for Verilog."
  '((module_declaration       . ts-fold-range-verilog-module)
    (list_of_port_connections . ts-fold-range-verilog-list)
    (initial_construct        . ts-fold-range-verilog-initial-construct)
    (comment                  . ts-fold-range-c-like-comment)))

(defun ts-fold-parsers-vhdl ()
  "Rule set for VHDL."
  '((package_declaration         . ts-fold-range-vhdl-package)
    (full_type_declaration       . ts-fold-range-vhdl-type)
    (enumeration_type_definition . ts-fold-range-seq)
    (comment                     . ts-fold-range-lua-comment)))

(defun ts-fold-parsers-xml ()
  "Rule set for XML."
  '((element . ts-fold-range-html)
    (Comment . (ts-fold-range-seq 3 -2))))

(defun ts-fold-parsers-yaml ()
  "Rule set for YAML."
  '((comment
     . (lambda (node offset)
         (ts-fold-range-line-comment node offset "#")))
    (block_mapping_pair
     . ((lambda (node offset)
          (ts-fold-range-markers node offset ":"))
        0 1))))

(defun ts-fold-parsers-zig ()
  "Rule set for Zig."
  '((Block        . ts-fold-range-seq)
    (line_comment . ts-fold-range-c-like-comment)))

(provide 'ts-fold-parsers)
;;; ts-fold-parsers.el ends here
