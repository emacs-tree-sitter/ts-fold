;;; ts-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Code folding using tree-sitter
;; Keyword: folding tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.9.0") (fringe-helper "1.0.1"))
;; URL: https://github.com/jcs090218/ts-fold

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
;; package.  Turn on the minor-mode `ts-fold-mode' to enable
;; this mechanism.  Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `ts-fold-mode' can properly fold codes.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 's)
(require 'tree-sitter)

(require 'ts-fold-util)
(require 'ts-fold-parsers)
(require 'ts-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup ts-fold nil
  "Code folding using tree-sitter."
  :group 'tree-sitter
  :prefix "ts-fold-")

(defvar ts-fold-foldable-node-alist nil
  "Collect a list of foldable node from variable `ts-fold-range-alist'.

The alist is in form of (major-mode . (foldable-node-type)).")

(defcustom ts-fold-range-alist
  `((agda-mode       . ,(ts-fold-parsers-agda))
    (sh-mode         . ,(ts-fold-parsers-bash))
    (c-mode          . ,(ts-fold-parsers-c))
    (c++-mode        . ,(ts-fold-parsers-c++))
    (csharp-mode     . ,(ts-fold-parsers-csharp))
    (css-mode        . ,(ts-fold-parsers-css))
    (ess-r-mode      . ,(ts-fold-parsers-r))
    (go-mode         . ,(ts-fold-parsers-go))
    (html-mode       . ,(ts-fold-parsers-html))
    (java-mode       . ,(ts-fold-parsers-java))
    (javascript-mode . ,(ts-fold-parsers-javascript))
    (js-mode         . ,(ts-fold-parsers-javascript))
    (js2-mode        . ,(ts-fold-parsers-javascript))
    (js3-mode        . ,(ts-fold-parsers-javascript))
    (json-mode       . ,(ts-fold-parsers-json))
    (jsonc-mode      . ,(ts-fold-parsers-json))
    (nix-mode        . ,(ts-fold-parsers-nix))
    (php-mode        . ,(ts-fold-parsers-php))
    (python-mode     . ,(ts-fold-parsers-python))
    (rjsx-mode       . ,(ts-fold-parsers-javascript))
    (ruby-mode       . ,(ts-fold-parsers-ruby))
    (rust-mode       . ,(ts-fold-parsers-rust))
    (rustic-mode     . ,(ts-fold-parsers-rust))
    (scala-mode      . ,(ts-fold-parsers-scala))
    (swift-mode      . ,(ts-fold-parsers-swift))
    (typescript-mode . ,(ts-fold-parsers-typescript)))
  "An alist of (major-mode . (foldable-node-type . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `ts-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq ts-fold-foldable-node-alist
               (let (alist)
                 (dolist (item ts-fold-range-alist)
                   (let ((mode (car item)) nodes)
                     (dolist (rule (cdr item)) (push (car rule) nodes))
                     (push (cons mode nodes) alist)))
                 alist)))
  :group 'ts-fold)

(defcustom ts-fold-mode-hook nil
  "Hook to run when enabling `ts-fold-mode`."
  :type 'hook
  :group 'ts-fold)

(defcustom ts-fold-replacement "..."
  "Show this string instead of the folded text."
  :type 'string
  :group 'ts-fold)

(defface ts-fold-replacement-face
  '((t :foreground "#808080" :box '(:line-width -1 :style 'pressed-button)))
  "Face used to display the fold replacement text."
  :group 'ts-fold)

(defface ts-fold-fringe-face
  '((t ()))
  "Face used to display fringe contents."
  :group 'ts-fold)

;;
;; (@* "Externals" )
;;

(declare-function ts-fold-indicators-refresh "ts-fold-indicators.el")

;;
;; (@* "Entry" )
;;

(defun ts-fold--enable ()
  "Start folding minor mode."
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(ts-fold . t))

  ;; evil integration
  (when (bound-and-true-p evil-fold-list)
    (add-to-list 'evil-fold-list
                 '((ts-fold-mode)
		   :toggle ts-fold-toggle
                   :open ts-fold-open
                   :close ts-fold-close
                   :open-rec ts-fold-open-recursively
                   :open-all ts-fold-open-all
                   :close-all ts-fold-close-all)))

  (run-hooks 'ts-fold-mode-hook))

(defun ts-fold--disable ()
  "Stop folding minor mode."
  (remove-from-invisibility-spec '(ts-fold . t))
  (let ((tree-sitter-mode t))
    (ts-fold-open-all)))

;;;###autoload
(define-minor-mode ts-fold-mode
  "Folding code using tree sitter."
  :init-value nil
  :lighter "TS-Fold"
  (if ts-fold-mode (ts-fold--enable) (ts-fold--disable)))

;;;###autoload
(define-global-minor-mode global-ts-fold-mode ts-fold-mode
  (lambda () (ts-fold-mode 1)))

;;
;; (@* "Core" )
;;

(defun ts-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.

Raise `user-error' if no foldable node is found.

This function is borrowed from `tree-sitter-node-at-point'."
  (let* ((pos (or pos (point)))
         (foldable-types (alist-get major-mode ts-fold-foldable-node-alist))
         (root (tsc-root-node tree-sitter-tree))
         (node (tsc-get-descendant-for-position-range root pos pos)))
    (let ((current node) result)
      (while current
        (if (memq (tsc-node-type current) foldable-types)
            (setq result current
                  current nil)
          (setq current (tsc-get-parent current))))
      (or result (user-error "No foldable node found at POS")))))

(defun ts-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE."
  (when-let* ((fold-alist (alist-get major-mode ts-fold-range-alist))
              (item (alist-get (tsc-node-type node) fold-alist)))
    (cond ((functionp item) (funcall item node (cons 0 0)))
          ((listp item) (funcall (nth 0 item) node (cons (nth 1 item) (nth 2 item))))
          (t (user-error "Current node is not found in `ts-fold-range-alist' in %s" major-mode)))))

;;
;; (@* "Overlays" )
;;

(defun ts-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when range
    (let* ((beg (car range)) (end (cdr range)) (ov (make-overlay beg end)))
      (overlay-put ov 'creator 'ts-fold)
      (overlay-put ov 'invisible 'ts-fold)
      (overlay-put ov 'display (or (and ts-fold-summary-show
                                        (ts-fold-summary--get (buffer-substring beg end)))
                                   ts-fold-replacement))
      (overlay-put ov 'face 'ts-fold-replacement-face)
      (overlay-put ov 'isearch-open-invisible #'ts-fold--isearch-open))))

(defun ts-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun ts-fold-overlay-at (node)
  "Return the ts-fold overlay at NODE if NODE is foldable and folded.
Return nil otherwise."
  (when-let* ((foldable-types (alist-get major-mode ts-fold-foldable-node-alist))
              ((memq (tsc-node-type node) foldable-types))
              (range (ts-fold--get-fold-range node)))
    (thread-last (overlays-in (car range) (cdr range))
      (seq-filter (lambda (ov)
                    (and (eq (overlay-get ov 'invisible) 'ts-fold)
                         (= (overlay-start ov) (car range))
                         (= (overlay-end ov) (cdr range)))))
      car)))

;;
;; (@* "Commands" )
;;

(defmacro ts-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Ignored, tree-sitter-mode is not enable in the current buffer")))

;;;###autoload
(defun ts-fold-close (&optional node)
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `ts-fold-foldable-node-alist' for the
current `major-mode'.  If no foldable NODE is found in point, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (let ((node (or node (ts-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (ts-fold-overlay-at node)))
        (delete-overlay ov))
      (ts-fold--create-overlay (ts-fold--get-fold-range node)))))

;;;###autoload
(defun ts-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((node (ts-fold--foldable-node-at-pos))
                (ov (ts-fold-overlay-at node)))
      (delete-overlay ov))))

;;;###autoload
(defun ts-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at point."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((node (ts-fold--foldable-node-at-pos))
                (beg (tsc-node-start-position node))
                (end (tsc-node-end-position node)))
      (thread-last (overlays-in beg end)
        (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'ts-fold)))
        (mapc #'delete-overlay)))))

;;;###autoload
(defun ts-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (ts-fold--ensure-ts
    (let* ((node (tsc-root-node tree-sitter-tree))
           (patterns (seq-mapcat (lambda (type) `(,(list type) @name))
                                 (alist-get major-mode ts-fold-foldable-node-alist)
                                 'vector))
           (query (tsc-make-query tree-sitter-language patterns))
           (nodes-to-fold (tsc-query-captures query node #'ignore)))
      (thread-last nodes-to-fold
        (mapcar #'cdr)
        (mapc #'ts-fold-close)))))

;;;###autoload
(defun ts-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (ts-fold--ensure-ts
    (thread-last (overlays-in (point-min) (point-max))
      (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'ts-fold)))
      (mapc #'delete-overlay))))

;;;###autoload
(defun ts-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (if-let* ((node (ts-fold--foldable-node-at-pos (point)))
              (ov (ts-fold-overlay-at node)))
        (progn (delete-overlay ov) t)
      (ts-fold-close))))

(defun ts-fold--after-command (&rest _)
  "Function call after interactive commands."
  (ts-fold-indicators-refresh))

(let ((commands '(ts-fold-close
                  ts-fold-open
                  ts-fold-open-recursively
                  ts-fold-close-all
                  ts-fold-open-all
                  ts-fold-toggle)))
  (dolist (command commands)
    (advice-add command :after #'ts-fold--after-command)))

;;
;; (@* "Rule Helpers" )
;;

(defun ts-fold--next-prev-node (node next)
  "Return previous/next sibling node starting from NODE.

If NEXT is non-nil, return next sibling.  Otherwirse, return previouse sibling."
  (if next (tsc-get-next-sibling node) (tsc-get-prev-sibling node)))

(defun ts-fold--continuous-node-prefix (node prefix next)
  "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
  (let ((iter-node node) (last-node node)
        (last-line (car (tsc-node-start-point node))) line text break
        (line-range 1) (last-line-range 1) max-line-range)
    (while (and iter-node (not break))
      (setq text (tsc-node-text iter-node)
            line (car (tsc-node-start-point iter-node))
            line-range (1+ (s-count-matches "\n" text))
            max-line-range (max line-range last-line-range))
      (if (and (ts-fold-util--in-range-p line (- last-line max-line-range) (+ last-line max-line-range))
               (string-prefix-p prefix text))
          (setq last-node iter-node last-line line
                last-line-range (1+ (s-count-matches "\n" text)))
        (setq break t))
      (setq iter-node (ts-fold--next-prev-node iter-node next)))
    last-node))

(defun ts-fold-range-seq (node offset)
  "Return the fold range in sequence starting from NODE.

Argument OFFSET can be used to tweak the final beginning and end position."
  (let ((beg (1+ (tsc-node-start-position node)))
        (end (1- (tsc-node-end-position node))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-line-comment (node offset prefix)
  "Define fold range for line comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information.

Argument PREFIX is the comment prefix in string."
  (when-let* ((first-node (ts-fold--continuous-node-prefix node prefix nil))
              (last-node (ts-fold--continuous-node-prefix node prefix t))
              (prefix-len (length prefix))
              (beg (+ (tsc-node-start-position first-node) prefix-len))
              (end (tsc-node-end-position last-node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-block-comment (node offset)
  "Define fold range for block comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (ts-fold-range-seq node (ts-fold-util--cons-add '(1 . -1) offset)))

(defun ts-fold-range-c-like-comment (node offset)
  "Define fold range for C-like comemnt."
  (let ((text (tsc-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "/*" text))
        (ts-fold-range-block-comment node offset)
      (if (string-prefix-p "///" text)
          (ts-fold-range-line-comment node offset "///")
        (ts-fold-range-line-comment node offset "//")))))

;;
;; (@* "Languages" )
;;

(defun ts-fold-range-c-preproc-if (node offset)
  "Define fold range for `if' preprocessor."
  (let* ((named-node (tsc-get-child-by-field node :condition))
         (else (tsc-get-child-by-field node :alternative))
         (beg (tsc-node-end-position named-node))
         (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-ifdef (node offset)
  "Define fold range for `ifdef' and `ifndef' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :name))
              (else (tsc-get-child-by-field node :alternative))
              (beg (tsc-node-end-position named-node))
              (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-elif (node offset)
  "Define fold range for `elif' preprocessor."
  (when-let* ((named-node (tsc-get-child-by-field node :condition))
              (else (tsc-get-child-by-field node :alternative))
              (beg (tsc-node-end-position named-node))
              (end (1- (tsc-node-start-position else))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-else (node offset)
  "Define fold range for `else' preprocessor."
  (when-let* ((target "#else")
              (len (length target))
              (beg (+ (tsc-node-start-position node) len))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-html (node offset)
  "Define fold range for tag in HTML."
  (let* ((beg (tsc-node-end-position (tsc-get-nth-child node 0)))
         (end-node (tsc-get-nth-child node (1- (tsc-count-children node))))
         (end (tsc-node-start-position end-node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-python (node offset)
  "Define fold range for `function_definition' and `class_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (tsc-get-child-by-field node :superclasses)
                              (tsc-get-child-by-field node :return_type)
                              (tsc-get-child-by-field node :parameters)
                              (tsc-get-child-by-field node :name)))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (tsc-node-end-position (tsc-get-next-sibling named-node)))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-ruby (node offset)
  "Define fold range for `method' and `class' in Ruby.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (tsc-get-child-by-field node :superclass)
                              (tsc-get-child-by-field node :parameters)
                              (tsc-get-child-by-field node :name)))
              (beg (tsc-node-end-position named-node))
              (end (tsc-node-end-position node)))
    (ts-fold-util--cons-add (cons beg end) offset)))

(defun ts-fold-range-rust-macro (node offset)
  "Return the fold range for `macro_definition' NODE in Rust.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((children (tsc-count-children node))
              (last_bracket (tsc-get-nth-child node (- children 1)))
              (first_bracket (tsc-get-nth-child node 2))
              (beg (tsc-node-start-position first_bracket))
              (end (1+ (tsc-node-start-position last_bracket))))
    (ts-fold-util--cons-add (cons beg end) offset)))

(provide 'ts-fold)
;;; ts-fold.el ends here
