;;; tree-sitter-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Code folding using tree-sitter
;; Keyword: folding tree-sitter
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1"))
;; URL: https://github.com/jcs090218/tree-sitter-fold

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
;; This package provides a code-folding mechanism based on tree-sitter
;; package.  Turn on the minor-mode `tree-sitter-fold-mode' to enable
;; this mechanism.  Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `tree-sitter-fold-mode' can properly fold codes.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'tree-sitter)
(require 'tree-sitter-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup tree-sitter-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "tree-sitter-fold-")

(defcustom tree-sitter-fold-foldable-node-alist
  '((c-mode          . (compound_statement declaration_list enumerator_list field_declaration_list comment))
    (c++-mode        . (compound_statement declaration_list enumerator_list field_declaration_list comment))
    (ess-r-mode      . (brace_list))
    (go-mode         . (type_declaration function_declaration method_declaration))
    (javascript-mode . (export_clause))
    (js-mode         . (export_clause))
    (js2-mode        . (export_clause named_imports statement_block switch_body object object_pattern))
    (js3-mode        . (export_clause))
    (rjsx-mode       . (export_clause named_imports statement_block switch_body object object_pattern jsx_expression))
    (nix-mode        . (attrset function))
    (python-mode     . (function_definition class_definition))
    (typescript-mode . (export_clause)))
  "An alist of (mode . (list of tree-sitter-nodes considered foldable in this mode))."
  :type '(alist :key-type symbol :value-type (repeat symbol))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-range-alist
  '((c-mode          . ((compound_statement     . tree-sitter-fold-range-seq)
                        (declaration_list       . tree-sitter-fold-range-seq)
                        (enumerator_list        . tree-sitter-fold-range-seq)
                        (field_declaration_list . tree-sitter-fold-range-seq)
                        (comment                . tree-sitter-fold-range-seq)))
    (c++-mode        . ((compound_statement     . tree-sitter-fold-range-seq)
                        (declaration_list       . tree-sitter-fold-range-seq)
                        (enumerator_list        . tree-sitter-fold-range-seq)
                        (field_declaration_list . tree-sitter-fold-range-seq)
                        (comment                . tree-sitter-fold-range-seq)))
    (ess-r-mode      . ((brace_list . tree-sitter-fold-range-seq)))
    (go-mode         . ((type_declaration     . tree-sitter-fold-range-go-type-declaration)
                        (function_declaration . tree-sitter-fold-range-go-method)
                        (method_declaration   . tree-sitter-fold-range-go-method)))
    (javascript-mode . ((export_clause . tree-sitter-fold-range-seq)))
    (js-mode         . ((export_clause . tree-sitter-fold-range-seq)))
    (js2-mode        . ((export_clause   . tree-sitter-fold-range-seq)
                        (statement_block . tree-sitter-fold-range-seq)))
    (js3-mode        . ((export_clause . tree-sitter-fold-range-seq)))
    (rjsx-mode       . ((export_clause . tree-sitter-fold-range-seq)))
    (nix-mode        . ((attrset  . tree-sitter-fold-range-nix-attrset)
                        (function . tree-sitter-fold-range-nix-function)))
    (python-mode     . ((function_definition . tree-sitter-fold-range-python)
                        (class_definition    . tree-sitter-fold-range-python)))
    (typescript-mode . ((export_clause . tree-sitter-fold-range-seq))))
  "An alist of (major-mode . (foldable-node-type . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `tree-sitter-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-mode-hook nil
  "Hook to run when enabling `tree-sitter-fold-mode`."
  :type 'hook
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-replacement "..."
  "Show this string instead of the folded text."
  :type 'string
  :group 'tree-sitter-fold)

(defface tree-sitter-fold-replacement-face
  '((t :foreground "#808080" :box '(:line-width -1 :style 'pressed-button)))
  "Face used to display the fold replacement text."
  :group 'tree-sitter-fold)

;;
;; (@* "Entry" )
;;

(defun tree-sitter-fold--enable ()
  "Start folding minor mode."
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(tree-sitter-fold . t))

  ;; evil integration
  (if (bound-and-true-p evil-fold-list)
      (add-to-list 'evil-fold-list
                   '((tree-sitter-fold-mode)
                     :open tree-sitter-fold-open
                     :close tree-sitter-fold-close
                     :open-rec tree-sitter-fold-open-recursively
                     :open-all tree-sitter-fold-open-all
                     :close-all tree-sitter-fold-close-all)))

  (run-hooks 'tree-sitter-fold-mode-hook))

(defun tree-sitter-fold--disable ()
  "Stop folding minor mode."
  (remove-from-invisibility-spec '(tree-sitter-fold . t))
  (let ((tree-sitter-mode t))
    (tree-sitter-fold-open-all)))

(define-minor-mode tree-sitter-fold-mode
  "Folding code using tree sitter."
  :init-value nil
  :lighter "TS-Fold"
  (if tree-sitter-fold-mode (tree-sitter-fold--enable) (tree-sitter-fold--disable)))

;;
;; (@* "Core" )
;;

(defun tree-sitter-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.

Raise `user-error' if no foldable node is found.

This function is borrowed from `tree-sitter-node-at-point'."
  (let* ((pos (or pos (point)))
         (foldable-types (alist-get major-mode tree-sitter-fold-foldable-node-alist))
         (root (tsc-root-node tree-sitter-tree))
         (node (tsc-get-descendant-for-position-range root pos pos)))
    (let ((current node) result)
      (while current
        (if (memq (tsc-node-type current) foldable-types)
            (setq result current
                  current nil)
          (setq current (tsc-get-parent current))))
      (or result (user-error "No foldable node found at POS")))))

(defun tree-sitter-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE."
  (if-let* ((fold-alist (alist-get major-mode tree-sitter-fold-range-alist))
            (fn (alist-get (tsc-node-type node) fold-alist)))
      (if (functionp fn)
          (funcall fn node)
        (user-error "Current node is not found in `tree-sitter-fold-range-alist' in %s" major-mode))))

;;
;; (@* "Overlays" )
;;

(defun tree-sitter-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when (not (null range))
    (let* ((beg (car range)) (end (cdr range)) (ov (make-overlay beg end)))
      (overlay-put ov 'invisible 'tree-sitter-fold)
      (overlay-put ov 'display (or (and tree-sitter-fold-show-summary
                                        (tree-sitter-fold--get-summary (buffer-substring beg end)))
                                   tree-sitter-fold-replacement))
      (overlay-put ov 'face 'tree-sitter-fold-replacement-face)
      (overlay-put ov 'isearch-open-invisible #'tree-sitter-fold--isearch-open))))

(defun tree-sitter-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun tree-sitter-fold-overlay-at (node)
  "Return the tree-sitter-fold overlay at NODE if NODE is foldable and folded.
Return nil otherwise."
  (when-let* ((foldable-types (alist-get major-mode tree-sitter-fold-foldable-node-alist))
              (_ (memq (tsc-node-type node) foldable-types))
              (range (tree-sitter-fold--get-fold-range node)))
    (thread-last (overlays-in (car range) (cdr range))
      (seq-filter (lambda (ov)
                    (and (eq (overlay-get ov 'invisible) 'tree-sitter-fold)
                         (= (overlay-start ov) (car range))
                         (= (overlay-end ov) (cdr range)))))
      car)))

;;
;; (@* "Commands" )
;;

(defmacro tree-sitter-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enable in the current buffer")))

(defun tree-sitter-fold-close (&optional node)
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `tree-sitter-fold-foldable-node-alist' for the
current `major-mode'.  If no foldable NODE is found in point, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let ((node (or node (tree-sitter-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (tree-sitter-fold-overlay-at node)))
        (delete-overlay ov))
      (tree-sitter-fold--create-overlay (tree-sitter-fold--get-fold-range node)))))

(defun tree-sitter-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                (ov (tree-sitter-fold-overlay-at node)))
      (delete-overlay ov))))

(defun tree-sitter-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at point."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                (beg (tsc-node-start-position node))
                (end (tsc-node-end-position node)))
      (thread-last (overlays-in beg end)
        (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
        (mapc #'delete-overlay)))))

(defun tree-sitter-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let* ((node (tsc-root-node tree-sitter-tree))
           (patterns (seq-mapcat (lambda (type) `(,(list type) @name))
                                 (alist-get major-mode tree-sitter-fold-foldable-node-alist)
                                 'vector))
           (query (tsc-make-query tree-sitter-language patterns))
           (nodes-to-fold (tsc-query-captures query node #'ignore)))
      (thread-last nodes-to-fold
        (mapcar #'cdr)
        (mapc #'tree-sitter-fold-close)))))

(defun tree-sitter-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
      (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
      (mapc #'delete-overlay))))

(defun tree-sitter-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (let ((node (tree-sitter-fold--foldable-node-at-pos (point))))
      (if-let* ((ov (tree-sitter-fold-overlay-at node)))
          (delete-overlay ov)
        (tree-sitter-fold-close node)))))

;;
;; (@* "Languages" )
;;

(defun tree-sitter-fold-range-seq (node)
  "Return the fold range in sequence."
  (let ((beg (1+ (tsc-node-start-position node)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-python (node)
  "Return the fold range for `function_definition' and `class_definition'
NODE in Python."
  (let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                         (tsc-get-child-by-field node :return_type)
                         (tsc-get-child-by-field node :parameters)
                         (tsc-get-child-by-field node :name)))
         ;; the colon is an anonymous node after return_type or parameters node
         (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
         (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-attrset (node)
  "Return the fold range for `attrset' NODE in Nix express language."
  (let ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
        (end (1- (tsc-node-end-position node))))
    (cons beg end)))

(defun tree-sitter-fold-range-nix-function (node)
  "Return the fold range for `function' NODE in Nix express language."
  (let ((beg (thread-first node
               (tsc-get-child-by-field :formals)
               (tsc-get-next-sibling)
               (tsc-node-end-position)))
        (end (tsc-node-end-position node)))
    (cons beg end)))

(defun tree-sitter-fold-range-go-type-declaration (node)
  "Return the fold range for `type_declaration' NODE in Go language.
Only `struct_type' and `interface_type' nodes can be folded."
  (when-let* ((type-spec-node (tsc-get-nth-child node 1))
              ;; the type_spec node is not named in the Go grammar
              ;; so ensure that the 1-th child is a type_spec node
              (_ (eq (tsc-node-type type-spec-node) 'type_spec))
              (type-node (tsc-get-child-by-field type-spec-node :type))
              (type-node-type (tsc-node-type type-node)))
    (cond
     ;; only struct and interface types can be folded
     ((or (eq type-node-type 'struct_type)
          (eq type-node-type 'interface_type))
      ;; find the end of the "struct" or "interface" keyword
      (let ((beg (1+ (tsc-node-end-position (tsc-get-nth-child type-node 0))))
            (end (tsc-node-end-position node)))
        (cons beg end)))
     (t nil))))

(defun tree-sitter-fold-range-go-method (node)
  "Return the fold range for `method_declaration' NODE in Go language."
  (let* ((named-node (or (tsc-get-child-by-field node :result)
                         (tsc-get-child-by-field node :parameters)))
         (beg (1+ (tsc-node-end-position named-node)))
         (end (tsc-node-end-position node)))
    (cons beg end)))

(provide 'tree-sitter-fold)
;;; tree-sitter-fold.el ends here
