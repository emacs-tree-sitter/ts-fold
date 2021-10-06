;;; tree-sitter-fold-indicators.el --- Display indicators for folding range  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-10-04 20:03:12

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
;; Display indicators for folding range
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'fringe-helper)

(require 'tree-sitter-fold-util)
(require 'tree-sitter-fold)

(defcustom tree-sitter-fold-indicators-fringe 'left-fringe
  "Display indicators on the left/right fringe."
  :type '(choice (const :tag "On the right fringe" right-fringe)
                 (const :tag "On the left fringe" left-fringe))
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-indicators-priority 30
  "Indicators fringe priority."
  :type 'integer
  :group 'tree-sitter-fold)

(defcustom tree-sitter-fold-indicators-face-function nil
  "Function call when apply to indicators face."
  :type 'function
  :group 'tree-sitter-fold)

(fringe-helper-define 'tree-sitter-fold-indicators-fr-plus nil
  "XXXXXXX"
  "X.....X"
  "X..X..X"
  "X.XXX.X"
  "X..X..X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'tree-sitter-fold-indicators-fr-minus-tail nil
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'tree-sitter-fold-indicators-fr-center nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX...")

(fringe-helper-define 'tree-sitter-fold-indicators-fr-end-left nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XXXXX" "...XXXXX"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(fringe-helper-define 'tree-sitter-fold-indicators-fr-end-right nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "XXXXX..." "XXXXX..."
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

;;
;; (@* "Entry" )
;;

(defvar tree-sitter-fold-indicators-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] #'tree-sitter-fold-indicators-click-fringe)
    (define-key map [right-fringe mouse-1] #'tree-sitter-fold-indicators-click-fringe)
    map)
  "Keymap for function `tree-sitter-fold-indicators-mode'.")

(defun tree-sitter-fold-indicators--enable ()
  "Enable `tree-sitter-fold-indicators' mode."
  (if (tree-sitter-fold-mode 1)  ; Enable `tree-sitter-fold-mode' automatically
      (progn
        (add-hook 'tree-sitter-after-change-functions #'tree-sitter-fold-indicators-refresh nil t)
        (add-hook 'after-save-hook #'tree-sitter-fold-indicators-refresh nil t))
    (tree-sitter-fold-indicators-mode -1)))

(defun tree-sitter-fold-indicators--disable ()
  "Disable `tree-sitter-fold-indicators' mode."
  (remove-hook 'tree-sitter-after-change-functions #'tree-sitter-fold-indicators-refresh t)
  (remove-hook 'after-save-hook #'tree-sitter-fold-indicators-refresh t)
  (tree-sitter-fold-indicators--remove-overlays))

;;;###autoload
(define-minor-mode tree-sitter-fold-indicators-mode
  "Minor mode for indicators mode."
  :group 'tree-sitter-fold-indicators
  :lighter nil
  :keymap tree-sitter-fold-indicators-mode-map
  :init-value nil
  (if tree-sitter-fold-indicators-mode (tree-sitter-fold-indicators--enable)
    (tree-sitter-fold-indicators--disable)))

;;;###autoload
(define-global-minor-mode global-tree-sitter-fold-indicators-mode tree-sitter-fold-indicators-mode
  (lambda () (tree-sitter-fold-indicators-mode 1)))

;;
;; (@* "Events" )
;;

(defun tree-sitter-fold-indicators-click-fringe (event)
  "EVENT click on fringe."
  (interactive "e")
  (let ((current-fringe (nth 1 (car (cdr event)))) ovs ov cur-ln)
    (when (eq current-fringe tree-sitter-fold-indicators-fringe)
      (mouse-set-point event)
      (beginning-of-line)
      (setq cur-ln (line-number-at-pos (point)))
      (setq ovs (append (tree-sitter-fold-util--overlays-in 'type 'tree-sitter-fold-indicators-fr-plus)
                        (tree-sitter-fold-util--overlays-in 'type 'tree-sitter-fold-indicators-fr-minus-tail)))
      (when ovs
        (setq ov (cl-some
                  (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                  ovs))
        (when ov
          (or (save-excursion
                (end-of-line)
                (when (nth 4 (syntax-ppss)) (back-to-indentation))
                (tree-sitter-fold-toggle))
              (tree-sitter-fold-toggle)))))))

;;
;; (@* "Core" )
;;

(defun tree-sitter-fold-indicators--create-overlay-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'creator 'tree-sitter-fold-indicators)
    ov))

(defun tree-sitter-fold-indicators--create-overlays (beg end show)
  "Return a list of indicator overlays.

Argument BEG and END are range to create indicators.  Argument SHOW is the flag
defined folded region."
  (let (ov-lst)
    (save-excursion
      (goto-char beg)
      (while (and (<= (line-beginning-position) end) (not (eobp)))
        (push (tree-sitter-fold-indicators--create-overlay-at-point) ov-lst)
        (forward-line 1)))
    (tree-sitter-fold-indicators--update-overlays (reverse ov-lst) show)))

(defun tree-sitter-fold-indicators--get-priority (bitmap)
  "Get priority by BITMAP."
  (let ((prior tree-sitter-fold-indicators-priority))
    (cl-case bitmap
      (tree-sitter-fold-indicators-fr-plus (+ prior 2))
      (tree-sitter-fold-indicators-fr-minus-tail (+ prior 2))
      (tree-sitter-fold-indicators-fr-end-left (+ prior 1))
      (tree-sitter-fold-indicators-fr-end-right (+ prior 1))
      (t prior))))

(defun tree-sitter-fold-indicators--get-string (show ov bitmap)
  "Return the string properties for OV by SHOW and BITMAP."
  (let* ((face (or (and (functionp tree-sitter-fold-indicators-face-function)
                        (funcall tree-sitter-fold-indicators-face-function (overlay-start ov)))
                   'tree-sitter-fold-fringe-face))
         (str (propertize "." 'display `(,tree-sitter-fold-indicators-fringe ,bitmap ,face))))
    (if show str
      (cl-case bitmap
        (tree-sitter-fold-indicators-fr-plus str)
        (tree-sitter-fold-indicators-fr-minus-tail nil)
        (tree-sitter-fold-indicators-fr-end-left nil)
        (tree-sitter-fold-indicators-fr-end-right nil)
        (t nil)))))

(defun tree-sitter-fold-indicators--active-ov (show ov bitmap)
  "SHOW the indicator OV with BITMAP."
  (when (overlayp ov)
    (overlay-put ov 'tree-sitter-fold-indicators-active show)
    (overlay-put ov 'type bitmap)
    (overlay-put ov 'priority (tree-sitter-fold-indicators--get-priority bitmap))
    (overlay-put ov 'before-string (tree-sitter-fold-indicators--get-string show ov bitmap))))

(defun tree-sitter-fold-indicators--get-end-fringe ()
  "Return end fringe bitmap according to variable `tree-sitter-fold-indicators-fringe'."
  (cl-case tree-sitter-fold-indicators-fringe
    (left-fringe 'tree-sitter-fold-indicators-fr-end-left)
    (right-fringe 'tree-sitter-fold-indicators-fr-end-right)
    (t (user-error "Invalid indicators fringe type: %s" tree-sitter-fold-indicators-fringe))))

(defun tree-sitter-fold-indicators--update-overlays (ov-lst show)
  "SHOW indicators overlays OV-LST."
  (when-let* ((len (length ov-lst))
              ((> len 1))
              (len-1 (1- len))
              (first-ov (nth 0 ov-lst))
              (last-ov (nth len-1 ov-lst))
              (index 1))
    (tree-sitter-fold-indicators--active-ov
     show first-ov
     (if show 'tree-sitter-fold-indicators-fr-minus-tail
       'tree-sitter-fold-indicators-fr-plus))
    (tree-sitter-fold-indicators--active-ov show last-ov (tree-sitter-fold-indicators--get-end-fringe))
    (while (< index len-1)
      (tree-sitter-fold-indicators--active-ov show (nth index ov-lst) 'tree-sitter-fold-indicators-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Update" )
;;

(defun tree-sitter-fold-indicators--create (node)
  "Create indicators with NODE."
  (when-let* ((range (tree-sitter-fold--get-fold-range node))
              (beg (car range)) (end (cdr range)))
    (let ((show (not (tree-sitter-fold-overlay-at node))))
      (tree-sitter-fold-indicators--create-overlays beg end show))))

;;;###autoload
(defun tree-sitter-fold-indicators-refresh (&rest _)
  "Refresh indicators for all folding range."
  (when tree-sitter-fold-indicators-mode
    (tree-sitter-fold--ensure-ts
      (when-let* ((node (tsc-root-node tree-sitter-tree))
                  (patterns (seq-mapcat (lambda (type) `(,(list type) @name))
                                        (alist-get major-mode tree-sitter-fold-foldable-node-alist)
                                        'vector))
                  (query (ignore-errors
                           (tsc-make-query tree-sitter-language patterns)))
                  (nodes-to-fold (tsc-query-captures query node #'ignore)))
        (tree-sitter-fold-indicators--remove-overlays)
        (thread-last nodes-to-fold
          (mapcar #'cdr)
          (mapc #'tree-sitter-fold-indicators--create))))))

(defun tree-sitter-fold-indicators--remove-overlays ()
  "Remove all indicators overlays."
  (remove-overlays (point-min) (point-max) 'creator 'tree-sitter-fold-indicators))

(provide 'tree-sitter-fold-indicators)
;;; tree-sitter-fold-indicators.el ends here
