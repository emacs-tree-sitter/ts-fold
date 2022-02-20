;;; ts-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021-2022  Shen, Jen-Chieh

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

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 's)
(require 'tree-sitter)

(require 'ts-fold-util)
(require 'ts-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup ts-fold nil
  "Code folding using tree-sitter."
  :prefix "ts-fold-"
  :group 'tree-sitter)

(defconst ts-fold--dir (file-name-directory (locate-library "ts-fold.el"))
  "The directory where the library `ts-fold' is located.")

(defconst ts-fold--queries-dir
  (file-name-as-directory (concat ts-fold--dir "queries"))
  "The directory where the `ts-fold' queries is located.")

(defcustom ts-fold-major-mode-language-alist nil
  "Alist that maps major modes to tree-sitter language names."
  :type '(alist :key-type symbol :value-type string)
  :group 'ts-fold)

(pcase-dolist
    (`(,major-mode . ,lang-symbol)
     (reverse
      '((sh-mode           . "bash")
        (shell-script-mode . "bash")
        (c-mode            . "c")
        (csharp-mode       . "c-sharp")
        (c++-mode          . "cpp")
        (go-mode           . "go")
        (html-mode         . "html")
        (java-mode         . "java")
        (javascript-mode   . "javascript")
        (js-mode           . "javascript")
        (js2-mode          . "javascript")
        (js3-mode          . "javascript")
        (julia-mode        . "julia")
        (php-mode          . "php")
        (python-mode       . "python")
        (rjsx-mode         . "javascript")
        (ruby-mode         . "ruby")
        (rust-mode         . "rust")
        (rustic-mode       . "rust")
        (typescript-mode   . "typescript"))))
  (setf (map-elt ts-fold-major-mode-language-alist major-mode) lang-symbol))

(defcustom ts-fold-groups-alist
  '((sh-mode           . ())
    (shell-script-mode . ())
    (c-mode            . ())
    (csharp-mode       . ("class .inner" "function .inner" "loop .inner" "conditional .inner" "block.inner"))
    (c++-mode          . ())
    (go-mode           . ())
    (html-mode         . ())
    (java-mode         . ())
    (javascript-mode   . ())
    (js-mode           . ())
    (js2-mode          . ())
    (js3-mode          . ())
    (julia-mode        . ())
    (php-mode          . ())
    (python-mode       . ())
    (rjsx-mode         . ())
    (ruby-mode         . ())
    (rust-mode         . ())
    (rustic-mode       . ())
    (typescript-mode   . ()))
  ""
  :type 'list
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
;; (@* "Queries" )
;;

(defun ts-fold--get-query (language top-level)
  "Get tree sitter query for LANGUAGE.
TOP-LEVEL is used to mention if we should load optional inherits."
  (with-temp-buffer
    (when-let* ((filename (concat ts-fold--queries-dir
                                  ;;language "/fold.scm"))  ; TODO: replace this?
                                  language "/textobjects.scm"))
                ((file-exists-p filename)))
      (insert-file-contents filename)
      (goto-char (point-min))
      (when-let* ((first-line (thing-at-point 'line t))
                  (first-line-matches
                   (save-match-data
                     (when (string-match "^; *inherits *:? *\\([a-z_,()]+\\) *$"
                                         first-line)
                       (match-string 1 first-line)))))
        (insert
         (string-join
          (mapcar (lambda (x)
                    (if (string-prefix-p "(" x)
                        (when top-level
                          (ts-fold--get-query (substring x 1 -1)
                                              nil))
                      (ts-fold--get-query x nil)))
                  (split-string first-line-matches ","))
          "\n")))
      (buffer-string))))

;;
;; (@* "Core" )
;;

(defun ts-fold--nodes-before (nodes)
  "NODES which contain the current after them."
  (cl-remove-if-not (lambda (x)
                      (< (byte-to-position (cdr (tsc-node-byte-range x))) (point)))
                    nodes))

(defun ts-fold--nodes-within (nodes)
  "NODES which contain the current point inside them ordered inside out."
  (let ((byte-pos (position-bytes (point))))
    (sort (cl-remove-if-not (lambda (x)
                              (and (<= (car (tsc-node-byte-range x)) byte-pos)
                                   (< byte-pos (cdr (tsc-node-byte-range x)))))
                            nodes)
          (lambda (x y)
            (< (+ (abs (- byte-pos
                          (car (tsc-node-byte-range x))))
                  (abs (- byte-pos
                          (cdr (tsc-node-byte-range x)))))
               (+ (abs (- byte-pos
                          (car (tsc-node-byte-range y))))
                  (abs (- byte-pos
                          (cdr (tsc-node-byte-range y))))))))))

(defun ts-fold--nodes-after (nodes)
  "NODES which contain the current point before them ordered top to bottom."
  (cl-remove-if-not (lambda (x)
                      (> (byte-to-position (car (tsc-node-byte-range x))) (point)))
                    nodes))

(defun ts-fold--get-nodes (group query)
  "Get a list of viable nodes based on `GROUP' value.
They will be order with captures with point inside them first then the
ones that follow.  If a `QUERY' alist is provided, we make use of that
instead of the builtin query set."
  (let* ((lang-name (alist-get major-mode ts-fold-major-mode-language-alist))
         (debugging-query (if query (alist-get major-mode query)
                            (ts-fold--get-query lang-name t)))
         (root-node (tsc-root-node tree-sitter-tree))
         (query (tsc-make-query tree-sitter-language debugging-query))
         (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties))
         (filtered-captures (cl-remove-if-not (lambda (x)
                                                (member (car x) group))
                                              captures))
         (nodes (seq-map #'cdr filtered-captures)))
    (cl-remove-duplicates
     nodes
     :test (lambda (x y)
             (and (= (car (tsc-node-byte-range x)) (car (tsc-node-byte-range y)))
                  (= (cdr (tsc-node-byte-range x)) (cdr (tsc-node-byte-range y))))))))

(defun ts-fold--get-within-and-after (group count query)
  "Given a `GROUP' `QUERY' find `COUNT' number of nodes within in and after current point."
  (let* ((nodes (ts-fold--get-nodes group query))
         (nodes-within (ts-fold--nodes-within nodes))
         (nodes-after (ts-fold--nodes-after nodes))
         (filtered-nodes (append nodes-within nodes-after)))
    (when (> (length filtered-nodes) 0)
      (cl-subseq filtered-nodes 0 count))))

(defun ts-fold--range (count ts-group &optional query)
  "Get the range of the closeset item of type `TS-GROUP'.
`COUNT' is supported even thought it does not actually make sense in
most cases as if we do 3-in-func the selections will not be continues,
but we can only provide the start and end as of now which is what we
are doing.  If a `QUERY' alist is provided, we make use of that
instead of the builtin query set."
  (when-let ((nodes (ts-fold--get-within-and-after ts-group count query)))
    (let ((range-min (apply #'min
                            (seq-map (lambda (x)
                                       (car (tsc-node-byte-range x)))
                                     nodes)))
          (range-max (apply #'max
                            (seq-map (lambda (x)
                                       (cdr (tsc-node-byte-range x)))
                                     nodes))))
      ;; Have to compute min and max like this as we might have nested functions
      ;; We have to use `cl-callf byte-to-position` ot the positioning might be off for unicode chars
      (cons (cl-callf byte-to-position range-min) (cl-callf byte-to-position range-max)))))

(defun ts-fold--get-fold-range ()
  "Return fold range."
  (if-let* ((pt (point))
            (group (alist-get major-mode ts-fold-groups-alist))
            (groups (if (eq (type-of group) 'string)
                        (list group)
                      group))
            (interned-groups (mapcar #'intern groups))
            (range (ts-fold--range 1 interned-groups)))
      (when (and (<= (car range) pt) (<= pt (cdr range)))
        range)
    (message "[INFO] No region found, `%s`" group)))

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

(defun ts-fold-overlay-at (range)
  "Return the ts-fold overlay at NODE if NODE is foldable and folded.
Return nil otherwise."
  (thread-last (overlays-in (car range) (cdr range))
    (seq-filter (lambda (ov)
                  (and (eq (overlay-get ov 'invisible) 'ts-fold)
                       (= (overlay-start ov) (car range))
                       (= (overlay-end ov) (cdr range)))))
    car))

;;
;; (@* "Commands" )
;;

(defmacro ts-fold--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode) ,@body
     (user-error "Ignored, tree-sitter-mode is not enable in the current buffer")))

(defconst ts-fold-interactive-commands
  '(ts-fold-close
    ts-fold-open
    ts-fold-open-recursively
    ts-fold-close-all
    ts-fold-open-all
    ts-fold-toggle)
  "List of interactive commands.")

;;;###autoload
(defun ts-fold-close ()
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `ts-fold-foldable-node-alist' for the
current `major-mode'.  If no foldable NODE is found in point, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (when-let ((range (ts-fold--get-fold-range)))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (ts-fold-overlay-at range))) (delete-overlay ov))
      (ts-fold--create-overlay range))))

;;;###autoload
(defun ts-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (ts-fold--ensure-ts
    (when-let* ((range (ts-fold--get-fold-range))
                (ov (ts-fold-overlay-at range)))
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
    (if-let* ((range (ts-fold--get-fold-range))
              (ov (ts-fold-overlay-at range)))
        (progn (delete-overlay ov) t)
      (ts-fold-close))))

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
