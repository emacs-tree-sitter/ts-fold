;;; tree-sitter-fold-util.el --- Utility module  -*- lexical-binding: t; -*-

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
;; (@* "Macros" )
;;

(defmacro tree-sitter-fold-util--with-current-buffer (buffer-or-name &rest body)
  "Safe to use function `with-current-buffer'."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer-or-name)
     (with-current-buffer ,buffer-or-name (progn ,@body))))

;;
;; (@* "Overlay" )
;;

(defun tree-sitter-fold-util--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (unless beg (setq beg (point-min))) (unless end (setq end (point-max)))
  (let ((lst '()) (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

(provide 'tree-sitter-fold-util)
;;; tree-sitter-fold-util.el ends here
