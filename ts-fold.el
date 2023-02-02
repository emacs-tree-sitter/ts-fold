;;; ts-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021-2022  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-tree-sitter/ts-fold
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.9.0") (fringe-helper "1.0.1"))
;; Keywords: convenience folding tree-sitter

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

;; TODO(everyone): This is a bit messy, but try to keep this alist
;; alphabetically sorted
(defcustom ts-fold-range-alist
  `((agda-mode       . ,(ts-fold-parsers-agda))
    (c-mode          . ,(ts-fold-parsers-c))
    (c-ts-mode       . ,(ts-fold-parsers-c))
    (c++-mode        . ,(ts-fold-parsers-c++))
    (c++-ts-mode     . ,(ts-fold-parsers-c++))
    (caml-mode       . ,(ts-fold-parsers-ocaml))
    (csharp-mode     . ,(ts-fold-parsers-csharp))
    (css-mode        . ,(ts-fold-parsers-css))
    (elixir-mode     . ,(ts-fold-parsers-elixir))
    (ess-r-mode      . ,(ts-fold-parsers-r))
    (go-mode         . ,(ts-fold-parsers-go))
    (html-mode       . ,(ts-fold-parsers-html))
    (java-mode       . ,(ts-fold-parsers-java))
    (javascript-mode . ,(ts-fold-parsers-javascript))
    (js-mode         . ,(ts-fold-parsers-javascript))
    (js-ts-mode      . ,(ts-fold-parsers-javascript))
    (js2-mode        . ,(ts-fold-parsers-javascript))
    (js3-mode        . ,(ts-fold-parsers-javascript))
    (json-mode       . ,(ts-fold-parsers-json))
    (json-ts-mode    . ,(ts-fold-parsers-json))
    (jsonc-mode      . ,(ts-fold-parsers-json))
    (julia-mode      . ,(ts-fold-parsers-julia))
    (nix-mode        . ,(ts-fold-parsers-nix))
    (ocaml-mode      . ,(ts-fold-parsers-ocaml))
    (php-mode        . ,(ts-fold-parsers-php))
    (python-mode     . ,(ts-fold-parsers-python))
    (python-ts-mode  . ,(ts-fold-parsers-python))
    (rjsx-mode       . ,(ts-fold-parsers-javascript))
    (ruby-ts-mode    . ,(ts-fold-parsers-ruby))
    (rust-mode       . ,(ts-fold-parsers-rust))
    (rustic-mode     . ,(ts-fold-parsers-rust))
    (sh-mode         . ,(ts-fold-parsers-bash))
    (scala-mode      . ,(ts-fold-parsers-scala))
    (swift-mode      . ,(ts-fold-parsers-swift))
    (tuareg-mode     . ,(ts-fold-parsers-ocaml))
    (typescript-ts-mode . ,(ts-fold-parsers-typescript))
    (tsx-ts-mode . ,(ts-fold-parsers-typescript))
    (typescript-mode . ,(ts-fold-parsers-typescript))
    (typescript-ts-mode . ,(ts-fold-parsers-typescript))
    )
  "An alist of (major-mode . (foldable-node-type . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `ts-fold-range-python' for an example."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
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
                   :close-all ts-fold-close-all))))

(defun ts-fold--disable ()
  "Stop folding minor mode."
  (remove-from-invisibility-spec '(ts-fold . t))
  (let ((tree-sitter-mode t))
    (ts-fold-open-all)))

(defun ts-fold--tree-sitter-trigger ()
  "Turn `ts-fold-mode' on and off alongside `treesit'
when in a mode ts-fold can act on."
  (if (and (functionp 'treesit-buffer-root-node)
           (treesit-buffer-root-node)
           (ts-fold-usable-mode-p))
      (ts-fold-mode 1)
    (ts-fold-mode -1)))

;;;###autoload
(define-minor-mode ts-fold-mode
  "Folding code using tree sitter."
  :group 'ts-fold
  :init-value nil
  :lighter "TS-Fold"
  (if ts-fold-mode (ts-fold--enable) (ts-fold--disable)))

;;;###autoload
(define-minor-mode global-ts-fold-mode
  "Use `ts-fold-mode' wherever possible"
  :group 'ts-fold
  :init-value nil
  :lighter nil
  :global t
  (if global-ts-fold-mode
      (progn
        (add-hook 'prog-mode-hook #'ts-fold--tree-sitter-trigger)
        ;; try to turn on in all buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (ts-fold--tree-sitter-trigger))))
    (remove-hook 'prog-mode-hook #'ts-fold--tree-sitter-trigger)))

(defun ts-fold-usable-mode-p (&optional mode)
  "Return non-nil if `ts-fold' has defined folds for MODE."
  (let ((mode (or mode major-mode)))
    (alist-get mode ts-fold-range-alist)))

;;
;; (@* "Core" )
;;

(defun ts-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.

Return nil if no valid node is found.

This function is borrowed from `tree-sitter-node-at-point'."
  (let* ((pos (or pos (point)))
         (mode-ranges (alist-get major-mode ts-fold-range-alist))
         (root (treesit-buffer-root-node))
         (node (treesit-node-descendant-for-range root pos pos))
         ;; Used for looping
         (current node))
    (while (and current (not (ts-fold--get-fold-func current)))
      (setq current (treesit-node-parent current)))
    current))

(defun ts-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE.
Return nil if there is no fold to be made."
  (when-let* ((fold-alist (alist-get major-mode ts-fold-range-alist))
              (fold-func (ts-fold--get-fold-func node)))
    (cond ((functionp fold-func) (funcall fold-func node (cons 0 0)))
          ((listp fold-func) (funcall (nth 0 fold-func) node (cons (nth 1 fold-func) (nth 2 fold-func))))
          (t (user-error "Bad folding function for node")))))

;;
;; (@* "Overlays" )
;;

(defun ts-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when range
    (let* ((beg (car range))
           (end (cdr range))
           (ov (make-overlay beg end)))
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
  (when-let* ((range (ts-fold--get-fold-range node)))
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
  (declare (indent 0)
           (debug (&rest form)))
  `(if (and (functionp 'treesit-buffer-root-node) (treesit-buffer-root-node))
       (progn ,@body)
     (message "ts-fold: ignoring, because cannot parse current buffer with treesit")))

;;;###autoload
(defun ts-fold-close (&optional node)
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `ts-fold-range-alist' for the
current `major-mode'.

If no NODE is found in point, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((node (or node (ts-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (ts-fold-overlay-at node)))
        (delete-overlay ov))
      (when-let* ((range (ts-fold--get-fold-range node)))
        (ts-fold--create-overlay range)))))

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
                (beg (treesit-node-start node))
                (end (treesit-node-end node)))
      (thread-last (overlays-in beg end)
                   (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'ts-fold)))
                   (mapc #'delete-overlay)))))

;;;###autoload
(defun ts-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (ts-fold--ensure-ts
    (let* ((node (treesit-buffer-root-node))
           (patterns (mapconcat (lambda (fold-range) (concat "(" (symbol-name (car fold-range)) ") " "@name"))
                                 (alist-get major-mode ts-fold-range-alist) " "))
           (query (treesit-query-compile (treesit-node-language node) patterns))
           (nodes-to-fold (treesit-query-capture node query nil nil t)))
      (thread-last nodes-to-fold
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
  (if next (treesit-node-next-sibling node) (treesit-node-prev-sibling node)))

(defun ts-fold--continuous-node-prefix (node prefix next)
  "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
  (let ((iter-node node) (last-node node)
        (last-line (treesit-node-start node)) line text break
        (line-range 1) (last-line-range 1) max-line-range)
    (while (and iter-node (not break))
      (setq text (treesit-node-text iter-node)
            line (treesit-node-start iter-node)
            line-range (1+ (s-count-matches "\n" text))
            max-line-range (max line-range last-line-range))
      (if (and (ts-fold--in-range-p line (- last-line max-line-range) (+ last-line max-line-range))
               (string-prefix-p prefix text))
          (setq last-node iter-node last-line line
                last-line-range (1+ (s-count-matches "\n" text)))
        (setq break t))
      (setq iter-node (ts-fold--next-prev-node iter-node next)))
    last-node))

(defun ts-fold--one-liner-node (node)
  "Helper function to check if NODE is on one line only."
  (= (car (aref (treesit-node-start node) 2)) (car (aref (treesit-node-end node) 3))))

(defun ts-fold-range-seq (node offset)
  "Return the fold range in sequence starting from NODE.

Argument OFFSET can be used to tweak the final beginning and end position."
  (let ((beg (1+ (treesit-node-start node)))
        (end (1- (treesit-node-end node))))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-line-comment (node offset prefix)
  "Define fold range for line comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information.

Argument PREFIX is the comment prefix in string."
  (when-let* ((first-node (ts-fold--continuous-node-prefix node prefix nil))
              (last-node (ts-fold--continuous-node-prefix node prefix t))
              (prefix-len (length prefix))
              (beg (+ (treesit-node-start first-node) prefix-len))
              (end (treesit-node-end last-node)))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-block-comment (node offset)
  "Define fold range for block comment.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (ts-fold-range-seq node (ts-fold--cons-add '(1 . -1) offset)))

(defun ts-fold-range-c-like-comment (node offset)
  "Define fold range for C-like comemnt.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (let ((text (treesit-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "/*" text))
        (ts-fold-range-block-comment node offset)
      (if (string-prefix-p "///" text)
          (ts-fold-range-line-comment node offset "///")
        (ts-fold-range-line-comment node offset "//")))))

;;
;; (@* "Languages" )
;;

(defun ts-fold-range-c-preproc-if (node offset)
  "Define fold range for `if' preprocessor.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (let* ((named-node (treesit-node-child-by-field-name node "condition"))
         (else (treesit-node-child-by-field-name node "alternative"))
         (beg (treesit-node-end named-node))
         (end (1- (treesit-node-start else))))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-ifdef (node offset)
  "Define fold range for `ifdef' and `ifndef' preprocessor.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (treesit-node-child-by-field-name node "name"))
              (else (treesit-node-child-by-field-name node "alternative"))
              (beg (treesit-node-end named-node))
              (end (1- (treesit-node-start else))))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-elif (node offset)
  "Define fold range for `elif' preprocessor.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (treesit-node-child-by-field-name node "condition"))
              (else (treesit-node-child-by-field-name node "alternative"))
              (beg (treesit-node-end named-node))
              (end (1- (treesit-node-start else))))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-c-preproc-else (node offset)
  "Define fold range for `else' preprocessor.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((target "#else")
              (len (length target))
              (beg (+ (treesit-node-start node) len))
              (end (treesit-node-end node)))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-html (node offset)
  "Define fold range for tag in HTML.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (let* ((beg (treesit-node-end (ts-fold--get-nth-child node 0)))
         (end-node (ts-fold--get-nth-child node (1- (treesit-node-child-count node))))
         (end (treesit-node-start end-node)))
    (ts-fold--cons-add (cons beg end) offset)))

;;+ OCaml

(defun ts-fold-range-ocaml-comment (node offset)
  "Define fold range for `comment'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (unless (ts-fold--one-liner-node node)
    (when-let*
        ((text  (treesit-node-text node))
         (beg   (if (string-prefix-p "(* " text)
                    (+ 2 (treesit-node-start node))
                  (+ 3 (treesit-node-start node))))
         (end   (- (treesit-node-end node) 2)))
      (ts-fold--cons-add (cons beg end) offset))))

(defun ts-fold-range-ocaml-module-definition (node offset)
  "Define fold range for `module_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (unless (ts-fold--one-liner-node node)
    (when-let*
        ((module-binding (ts-fold--get-nth-child node 0))
         (body           (treesit-node-child-by-field-name module-binding "body"))
         ;; body is struct ... end
         (beg            (+ 6 (treesit-node-start body)))
         (end            (- (treesit-node-end node) 3)))
      (ts-fold--cons-add (cons beg end) offset))))

(defun ts-fold-range-ocaml-type-definition (node offset)
  "Define fold range for `type_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (unless (ts-fold--one-liner-node node)
    (when-let*
        ((type-definition (ts-fold--get-nth-named-child node 0))
         (body            (treesit-node-child-by-field-name type-definition "body"))
         (text            (treesit-node-text (ts-fold--get-nth-child body 0)))
         (beg
          (if (string-equal "{" text)
              (1+ (treesit-node-start body))
            (treesit-node-end (treesit-node-prev-sibling body))))
         (end
          (if (string-equal "{" text)
              (1- (treesit-node-end node))
            (treesit-node-end node))))
      (ts-fold--cons-add (cons beg end) offset))))

(defun ts-fold-range-ocaml-value-definition (node offset)
  "Define fold range for `value_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (unless (ts-fold--one-liner-node node)
    (when-let*
        ((let-binding  (ts-fold--get-nth-named-child node 0))
         (body         (treesit-node-child-by-field-name let-binding "body"))
         (beg          (treesit-node-end (treesit-node-prev-sibling body)))
         (end          (treesit-node-end node)))
      (ts-fold--cons-add (cons beg end) offset))))

;;- OCaml

(defun ts-fold-range-python (node offset)
  "Define fold range for `function_definition' and `class_definition'.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (treesit-node-child-by-field-name node "superclasses")
                              (treesit-node-child-by-field-name node "return_type")
                              (treesit-node-child-by-field-name node "parameters")
                              (treesit-node-child-by-field-name node "name")))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (treesit-node-end (treesit-node-next-sibling named-node)))
              (end (treesit-node-end node)))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-ruby-class-def (node offset)
  "Define fold range for `method' and `class' in Ruby.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((named-node (or (treesit-node-child-by-field-name node "superclass")
                              (treesit-node-child-by-field-name node "parameters")
                              (treesit-node-child-by-field-name node "name")))
              (beg (treesit-node-end named-node))
              (end (treesit-node-end node)))
    (ts-fold--cons-add (cons beg (- end 3)) offset)))

(defun ts-fold-range-ruby-if (node offset)
  "Define fold range for `if' (then), `elsif', and `else' in Ruby.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let ((beg (treesit-node-start node))
             (end (treesit-node-end node)))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-rust-macro (node offset)
  "Return the fold range for `macro_definition' NODE in Rust.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((children (treesit-node-child-count node))
              (last_bracket (ts-fold--get-nth-child node (- children 1)))
              (first_bracket (ts-fold--get-nth-child node 2))
              (beg (treesit-node-start first_bracket))
              (end (1+ (treesit-node-start last_bracket))))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-elixir (node offset)
  "Return the fold range for `function' `module' NODE in Elixir.

For arguments NODE and OFFSET, see function `ts-fold-range-seq' for
more information."
  (when-let* ((children (treesit-node-child-count node))
              (end_child (ts-fold--get-nth-child node (- children 1)))
              (do_child (ts-fold--get-nth-child node 1))
              (beg (treesit-node-start do_child))
              (end (treesit-node-start end_child)))
    (ts-fold--cons-add (cons beg end) offset)))

(defun ts-fold-range-julia (node offset)
  "Return the fold range for a NODE in Julia.

It excludes the NODE's first child and the `end' keyword.  For
argument OFFSET, see function `ts-fold-range-seq' for more
information."
  (let* ((identifier (ts-fold--get-nth-named-child node 0))
         (end-position (treesit-node-end identifier))
         (start-position (treesit-node-start node))
         (fold-begin (1- (- end-position start-position))))
    (ts-fold-range-seq node (ts-fold--cons-add (cons fold-begin -2) offset))))

(provide 'ts-fold)
;;; ts-fold.el ends here
