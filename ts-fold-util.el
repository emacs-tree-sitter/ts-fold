;;; ts-fold-util.el --- Utility module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
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

(defun ts-fold-util--cons-add (c1 c2)
  "Addition for two cons C1 and C2."
  (cons (+ (car c1) (car c2)) (+ (cdr c1) (cdr c2))))

;;
;; (@* "Overlay" )
;;

(defun ts-fold-util--overlays-in (prop name &optional beg end)
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

(defvar ts-fold-util--doc-faces
  '(font-lock-doc-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    tree-sitter-hl-face:comment
    tree-sitter-hl-face:doc
    hl-todo)
  "List of face that apply for document string.")

(defun ts-fold-util--get-face (obj trim)
  "Return face name from OBJ.
If argument TRIM is non-nil, trim the OBJ."
  (get-text-property 0 'face (if trim (string-trim obj) obj)))

(defun ts-fold-util--is-face (obj lst-face &optional trim)
  "Return non-nil if OBJ's face is define inside list LST-FACE.
Optional argument TRIM, see function `ts-fold-util--get-face'."
  (unless (listp lst-face) (setq lst-face (list lst-face)))
  (let ((faces (ts-fold-util--get-face obj trim)))
    (cond ((listp faces)
           (cl-some (lambda (face) (memq face lst-face)) faces))
          (t (memq faces lst-face)))))

(defun ts-fold-util--doc-faces-p (obj &optional trim)
  "Return non-nil if face at OBJ is within `ts-fold-util--doc-faces' list.
Optional argument TRIM, see function `ts-fold-util--get-face'."
  (ts-fold-util--is-face obj ts-fold-util--doc-faces trim))

;;
;; (@* "Math" )
;;

(defun ts-fold-util--in-range-p (in-val in-min in-max)
  "Check to see if IN-VAL is between IN-MIN and IN-MAX."
  (and (<= in-min in-val) (<= in-val in-max)))

(provide 'ts-fold-util)
;;; ts-fold-util.el ends here
