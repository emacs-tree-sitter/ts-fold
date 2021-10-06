;;; tree-sitter-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Code folding using tree-sitter
;; Keyword: folding tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.9.0") (fringe-helper "1.0.1"))
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

(require 'tree-sitter-fold-util)
(require 'tree-sitter-fold-parsers)
(require 'tree-sitter-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup tree-sitter-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "tree-sitter-fold-")

(defvar tree-sitter-fold-foldable-node-alist nil
  "Collect a list of foldable node from variable `tree-sitter-fold-range-alist'.

The alist is in form of (major-mode . (foldable-node-type)).")

(defcustom tree-sitter-fold-range-alist
  `((agda-mode       . ,(tree-sitter-fold-parsers-agda))
    (sh-mode         . ,(tree-sitter-fold-parsers-bash))
    (c-mode          . ,(tree-sitter-fold-parsers-c))
    (c++-mode        . ,(tree-sitter-fold-parsers-c++))
    (csharp-mode     . ,(tree-sitter-fold-parsers-csharp))
    (css-mode        . ,(tree-sitter-fold-parsers-css))
    (ess-r-mode      . ,(tree-sitter-fold-parsers-r))
    (go-mode         . ,(tree-sitter-fold-parsers-go))
    (html-mode       . ,(tree-sitter-fold-parsers-html))
    (mhtml-mode      . ,(tree-sitter-fold-parsers-html))
    (java-mode       . ,(tree-sitter-fold-parsers-java))
    (javascript-mode . ,(tree-sitter-fold-parsers-javascript))
    (js-mode         . ,(tree-sitter-fold-parsers-javascript))
    (js2-mode        . ,(tree-sitter-fold-parsers-javascript))
    (js3-mode        . ,(tree-sitter-fold-parsers-javascript))
    (json-mode       . ,(tree-sitter-fold-parsers-json))
    (jsonc-mode      . ,(tree-sitter-fold-parsers-json))
    (nix-mode        . ,(tree-sitter-fold-parsers-nix))
    (php-mode        . ,(tree-sitter-fold-parsers-php))
    (python-mode     . ,(tree-sitter-fold-parsers-python))
    (rjsx-mode       . ,(tree-sitter-fold-parsers-javascript))
    (ruby-mode       . ,(tree-sitter-fold-parsers-ruby))
    (rust-mode       . ,(tree-sitter-fold-parsers-rust))
    (rustic-mode     . ,(tree-sitter-fold-parsers-rust))
    (scala-mode      . ,(tree-sitter-fold-parsers-scala))
    (swift-mode      . ,(tree-sitter-fold-parsers-swift))
    (typescript-mode . ,(tree-sitter-fold-parsers-typescript)))
  "An alist of (major-mode . (foldable-node-type . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `tree-sitter-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq tree-sitter-fold-foldable-node-alist
               (let (alist)
                 (dolist (item tree-sitter-fold-range-alist)
                   (let ((mode (car item)) nodes)
                     (dolist (rule (cdr item)) (push (car rule) nodes))
                     (push (cons mode nodes) alist)))
                 alist)))
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

(defface tree-sitter-fold-fringe-face
  '((t ()))
  "Face used to display fringe contents."
  :group 'tree-sitter-fold)

;;
;; (@* "Externals" )
;;

(declare-function tree-sitter-fold-indicators-refresh "tree-sitter-fold-indicators.el")

;;
;; (@* "Entry" )
;;

(defun tree-sitter-fold--enable ()
  "Start folding minor mode."
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(tree-sitter-fold . t))

  ;; evil integration
  (when (bound-and-true-p evil-fold-list)
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

;;;###autoload
(define-minor-mode tree-sitter-fold-mode
  "Folding code using tree sitter."
  :init-value nil
  :lighter "TS-Fold"
  (if tree-sitter-fold-mode (tree-sitter-fold--enable) (tree-sitter-fold--disable)))

;;;###autoload
(define-global-minor-mode global-tree-sitter-fold-mode tree-sitter-fold-mode
  (lambda () (tree-sitter-fold-mode 1)))

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
  (when-let* ((fold-alist (alist-get major-mode tree-sitter-fold-range-alist))
              (item (alist-get (tsc-node-type node) fold-alist)))
    (cond ((functionp item) (funcall item node (cons 0 0)))
          ((listp item) (funcall (nth 0 item) node (cons (nth 1 item) (nth 2 item))))
          (t (user-error "Current node is not found in `tree-sitter-fold-range-alist' in %s" major-mode)))))

;;
;; (@* "Overlays" )
;;

(defun tree-sitter-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when range
    (let* ((beg (car range)) (end (cdr range)) (ov (make-overlay beg end)))
      (overlay-put ov 'creator 'tree-sitter-fold)
      (overlay-put ov 'invisible 'tree-sitter-fold)
      (overlay-put ov 'display (or (and tree-sitter-fold-summary-show
                                        (tree-sitter-fold-summary--get (buffer-substring beg end)))
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
              ((memq (tsc-node-type node) foldable-types))
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

;;;###autoload
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

;;;###autoload
(defun tree-sitter-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (when-let* ((node (tree-sitter-fold--foldable-node-at-pos))
                (ov (tree-sitter-fold-overlay-at node)))
      (delete-overlay ov))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun tree-sitter-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
      (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'tree-sitter-fold)))
      (mapc #'delete-overlay))))

;;;###autoload
(defun tree-sitter-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (tree-sitter-fold--ensure-ts
    (if-let* ((node (tree-sitter-fold--foldable-node-at-pos (point)))
              (ov (tree-sitter-fold-overlay-at node)))
        (progn (delete-overlay ov) t)
      (tree-sitter-fold-close))))

(defun tree-sitter-fold--after-command (&rest _)
  "Function call after interactive commands."
  (tree-sitter-fold-indicators-refresh))

(let ((commands '(tree-sitter-fold-close
                  tree-sitter-fold-open
                  tree-sitter-fold-open-recursively
                  tree-sitter-fold-close-all
                  tree-sitter-fold-open-all
                  tree-sitter-fold-toggle)))
  (dolist (command commands)
    (advice-add command :after #'tree-sitter-fold--after-command)))

;;
;; (@* "Rule Helpers" )
;;

(defun tree-sitter-fold--multi-line (node)
  "Return t, if content NODE is single line."
  (string-match-p "\n" (tsc-node-text node)))

(defun tree-sitter-fold--next-prev-node (node next)
  "Return previous/next sibling node starting from NODE.

If NEXT is non-nil, return next sibling.  Otherwirse, return previouse sibling."
  (if next (tsc-get-next-sibling node) (tsc-get-prev-sibling node)))

(defun tree-sitter-fold--get-node-by-text (node text next)
  "Return node with matching TEXT.
Argument NODE is the starting node."
  (let ((current (tree-sitter-fold--next-prev-node node next)) result)
    (while current
      (if (string= text (string-trim (tsc-node-text current)))
          (setq result current
                current nil)
        (setq current (tree-sitter-fold--next-prev-node current next))))
    result))

(defun tree-sitter-fold--continuous-node-prefix (node prefix next)
  "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
  (let ((iter-node (tree-sitter-fold--next-prev-node node next)) text break
        (last-node node))
    (while (and iter-node (not break))
      (setq text (tsc-node-text iter-node))
      (if (string-prefix-p prefix text)
          (setq last-node iter-node)
        (setq break t))
      (setq iter-node (tree-sitter-fold--next-prev-node iter-node next)))
    last-node))

(defun tree-sitter-fold-range-seq (node offset)
  "Return the fold range in sequence starting from NODE.

Argument OFFSET can be used to tweak the final beginning and end position."
  (let ((beg (1+ (tsc-node-start-position node)))
        (end (1- (tsc-node-end-position node))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-line-comment (node offset prefix)
  "Define fold range for line comment.

For arguments NODE and OFFSET, see function `tree-sitter-fold-range-seq' for
more information.

Argument PREFIX is the comment prefix in string."
  (when-let* ((first-node (tree-sitter-fold--continuous-node-prefix node prefix nil))
              (last-node (tree-sitter-fold--continuous-node-prefix node prefix t))
              (prefix-len (length prefix))
              (beg (+ (tsc-node-start-position first-node) prefix-len))
              (end (tsc-node-end-position last-node)))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-block-comment (node offset)
  "Define fold range for block comment.

For arguments NODE and OFFSET, see function `tree-sitter-fold-range-seq' for
more information."
  (tree-sitter-fold-range-seq node (tree-sitter-fold-util--cons-add '(1 . -1) offset)))

(defun tree-sitter-fold-c-like-comment (node offset)
  "Define fold range for C-like comemnt."
  (if (tree-sitter-fold--multi-line node)
      (tree-sitter-fold-range-block-comment node offset)
    (if (string-prefix-p "///" (tsc-node-text node))
        (tree-sitter-fold-range-line-comment node offset "///")
      (tree-sitter-fold-range-line-comment node offset "//"))))

;;
;; (@* "Languages" )
;;

(defun tree-sitter-fold-range-c-preproc-if (node offset)
  "Define fold range for `if' preprocessor."
  (let* ((named-node (tsc-get-child-by-field node :condition))
         (next (or (tree-sitter-fold--get-node-by-text named-node "#elif" t)
                   (tree-sitter-fold--get-node-by-text named-node "#else" t)
                   (tree-sitter-fold--get-node-by-text named-node "#endif" t)))
         (beg (tsc-node-end-position named-node))
         (end (1- (tsc-node-start-position next))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-c-preproc-ifdef (node offset)
  "Define fold range for `ifdef' and `ifndef' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :name))
              (next (or (tree-sitter-fold--get-node-by-text named-node "#elif" t)
                        (tree-sitter-fold--get-node-by-text named-node "#else" t)
                        (tree-sitter-fold--get-node-by-text named-node "#endif" t)))
              (beg (tsc-node-end-position named-node))
              (end (1- (tsc-node-start-position next))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-c-preproc-elif (node offset)
  "Define fold range for `elif' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :condition))
              (next (or (tree-sitter-fold--get-node-by-text named-node "#elif" t)
                        (tree-sitter-fold--get-node-by-text named-node "#else" t)
                        (tree-sitter-fold--get-node-by-text named-node "#endif" t)))
              (beg (tsc-node-end-position node))
              (end (1- (tsc-node-start-position next))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-c-preproc-else (node offset)
  "Define fold range for `else' preprocessor."
  (when-let* ((next (tree-sitter-fold--get-node-by-text node "#endif" t))
              (beg (tsc-node-end-position node))
              (end (1- (tsc-node-start-position next))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-python (node offset)
  "Define fold range for `function_definition' and `class_definition'.

For arguments NODE and OFFSET, see function `tree-sitter-fold-range-seq' for
more information."
  (when-let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                              (tsc-get-child-by-field node :return_type)
                              (tsc-get-child-by-field node :parameters)
                              (tsc-get-child-by-field node :name)))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
              (end (tsc-node-end-position node)))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(defun tree-sitter-fold-range-ruby (_node _offset)
  "Define fold range for Ruby.

For arguments NODE and OFFSET, see function `tree-sitter-fold-range-seq' for
more information."
  ;; TODO: ..
  (progn ))

(defun tree-sitter-fold-range-rust-macro (node offset)
  "Return the fold range for `macro_definition' NODE in Rust.

For arguments NODE and OFFSET, see function `tree-sitter-fold-range-seq' for
more information."
  (when-let* ((children (tsc-count-children node))
              (last_bracket (tsc-get-nth-child node (- children 1)))
              (first_bracket (tsc-get-nth-child node 2))
              (beg (tsc-node-start-position first_bracket))
              (end (1+ (tsc-node-start-position last_bracket))))
    (tree-sitter-fold-util--cons-add (cons beg end) offset)))

(provide 'tree-sitter-fold)
;;; tree-sitter-fold.el ends here
