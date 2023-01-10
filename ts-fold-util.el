;;; ts-fold-util.el --- Utility module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Shen, Jen-Chieh
;; Created date 2021-10-04 20:19:42

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
;; Utility module.
;;

;;; Code:

;;
;; (@* "Cons" )
;;

(defun ts-fold--cons-add (c1 c2)
  "Addition for two cons C1 and C2."
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

;;
;; (@* "Overlay" )
;;

(defun ts-fold--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (unless beg (setq beg (point-min))) (unless end (setq end (point-max)))
  (let ((lst '()) (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

;;
;; (@* "Face" )
;;

(defvar ts-fold--doc-faces
  '(font-lock-doc-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    tree-sitter-hl-face:comment
    tree-sitter-hl-face:doc
    hl-todo)
  "List of face that apply for document string.")

(defun ts-fold--get-face (obj trim)
  "Return face name from OBJ.
If argument TRIM is non-nil, trim the OBJ."
  (get-text-property 0 'face (if trim (string-trim obj) obj)))

(defun ts-fold--is-face (obj lst-face &optional trim)
  "Return non-nil if OBJ's face is define inside list LST-FACE.
Optional argument TRIM, see function `ts-fold--get-face'."
  (unless (listp lst-face) (setq lst-face (list lst-face)))
  (let ((faces (ts-fold--get-face obj trim)))
    (cond ((listp faces)
           (cl-some (lambda (face) (memq face lst-face)) faces))
          (t (memq faces lst-face)))))

(defun ts-fold--doc-faces-p (obj &optional trim)
  "Return non-nil if face at OBJ is within `ts-fold--doc-faces' list.
Optional argument TRIM, see function `ts-fold--get-face'."
  (ts-fold--is-face obj ts-fold--doc-faces trim))

;;
;; (@* "Math" )
;;

(defun ts-fold--in-range-p (in-val in-min in-max)
  "Check to see if IN-VAL is between IN-MIN and IN-MAX."
  (and (<= in-min in-val) (<= in-val in-max)))

;;
;; (@* "TS node" )
;;

(defun ts-fold--compare-type (node type)
  "Compare NODE's type to TYPE."
  ;; tsc-node-type returns a symbol or a string and `string=' automatically
  ;; converts symbols to strings
  (string= (tsc-node-type node) type))

(defun ts-fold-get-children (node)
  "Get list of direct children of NODE."
  (let (children)
    (dotimes (index (tsc-count-children node))
      (push (tsc-get-nth-child node index) children))
    (reverse children)))

(defun ts-fold-get-children-traverse (node)
  "Return children from NODE but traverse it."
  (let (nodes)
    (tsc-traverse-mapc (lambda (child) (push child nodes)) node)
    (reverse nodes)))

(defun ts-fold-find-children (node type)
  "Search through the children of NODE to find all with type equal to TYPE;
then return that list."
  (cl-remove-if-not (lambda (child) (ts-fold--compare-type child type))
                    (ts-fold-get-children node)))

(defun ts-fold-find-children-traverse (node type)
  "Like function `ts-fold-find-children' but traverse it."
  (cl-remove-if-not (lambda (child) (ts-fold--compare-type child type))
                    (ts-fold-get-children-traverse node)))

(defun ts-fold-find-parent (node type)
  "Find the TYPE of parent from NODE."
  (let ((parent (tsc-get-parent node))
        (break))
    (while (and parent (not break))
      (setq break (ts-fold--compare-type parent type))
      (unless break
        (setq parent (tsc-get-parent parent))))
    parent))

(provide 'ts-fold-util)
;;; ts-fold-util.el ends here
