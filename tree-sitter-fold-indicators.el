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

(require 'fringe-helper)

(require 'tree-sitter-fold-util)

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

(fringe-helper-define 'tree-sitter-fold-indicators-fr-minus nil
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
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
        (add-hook 'after-change-functions #'tree-sitter-fold-indicators--start-timer nil t)
        (add-hook 'after-save-hook #'tree-sitter-fold-indicators--start-timer nil t))
    (origami-indicators-mode -1)))

(defun tree-sitter-fold-indicators--disable ()
  "Disable `tree-sitter-fold-indicators' mode."
  (remove-hook 'after-change-functions #'tree-sitter-fold-indicators--start-timer t)
  (remove-hook 'after-save-hook #'tree-sitter-fold-indicators--start-timer t)
  (tree-sitter-fold-indicators--remove-overlays (current-buffer)))

;;;###autoload
(define-minor-mode origami-indicators-mode
  "Minor mode for indicators mode."
  :group 'origami-indicators
  :lighter nil
  :keymap origami-indicators-mode-map
  :init-value nil
  (if origami-indicators-mode (tree-sitter-fold-indicators--enable)
    (tree-sitter-fold-indicators--disable)))

;;;###autoload
(define-global-minor-mode global-origami-indicators-mode origami-indicators-mode
  (lambda () (origami-indicators-mode 1)))

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
                        (tree-sitter-fold-util--overlays-in 'type 'tree-sitter-fold-indicators-fr-minus)
                        (tree-sitter-fold-util--overlays-in 'type 'tree-sitter-fold-indicators-fr-minus-tail)))
      (when ovs
        (setq ov (cl-some
                  (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                  ovs))
        (when ov
          (end-of-line)
          (call-interactively #'tree-sitter-fold-toggle))))))

;;
;; (@* "Core" )
;;

(defun origami-indicators--create-overlay-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'creator 'origami-indicators)
    ov))

(defun origami-indicators--create-overlays (beg end)
  "Return a list of indicator overlays from BEG to END."
  (let ((ov-lst '()))
    (save-excursion
      (goto-char beg)
      (while (and (<= (line-beginning-position) end) (not (eobp)))
        (push (origami-indicators--create-overlay-at-point) ov-lst)
        (forward-line 1)))
    (origami-indicators--update-overlays (reverse ov-lst) t)))

(defun tree-sitter-fold-indicators--get-priority (bitmap)
  "Get priority by BITMAP."
  (let ((prior tree-sitter-fold-indicators-priority))
    (cl-case bitmap
      (tree-sitter-fold-indicators-fr-plus (+ prior 2))
      (tree-sitter-fold-indicators-fr-minus (+ prior 2))
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
        (tree-sitter-fold-indicators-fr-minus nil)
        (tree-sitter-fold-indicators-fr-minus-tail nil)
        (tree-sitter-fold-indicators-fr-end-left nil)
        (tree-sitter-fold-indicators-fr-end-right nil)
        (t nil)))))

(defun origami-indicators--active-ov (show ov bitmap)
  "SHOW the indicator OV with BITMAP."
  (when (overlayp ov)
    (overlay-put ov 'origami-indicators-active show)
    (overlay-put ov 'type bitmap)
    (overlay-put ov 'priority (tree-sitter-fold-indicators--get-priority bitmap))
    (overlay-put ov 'before-string (tree-sitter-fold-indicators--get-string show ov bitmap))))

(defun origami-indicators--get-end-fringe ()
  "Return end fringe bitmap according to variable `tree-sitter-fold-indicators-fringe'."
  (cl-case tree-sitter-fold-indicators-fringe
    (left-fringe 'origami-indicators-fr-end-left)
    (right-fringe 'origami-indicators-fr-end-right)
    (t (user-error "Invalid indicators fringe type: %s" tree-sitter-fold-indicators-fringe))))

(defun origami-indicators--update-overlays (ov-lst show)
  "SHOW indicators overlays OV-LST."
  (let* ((len (length ov-lst))
         (len-1 (1- len))
         (first-ov (nth 0 ov-lst))
         (last-ov (nth len-1 ov-lst))
         (index 1))
    (origami-indicators--active-ov
     show first-ov
     (if show
         (if (> len 1)
             'origami-indicators-fr-minus-tail 'origami-indicators-fr-minus)
       'origami-indicators-fr-plus))
    (when (> len 1)
      (origami-indicators--active-ov show last-ov (origami-indicators--get-end-fringe)))
    (while (< index len-1)
      (origami-indicators--active-ov show (nth index ov-lst) 'origami-indicators-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Timer" )
;;

(defcustom tree-sitter-fold-indicators-time 0.5
  "Indicators refresh rate in time."
  :type 'float
  :group 'tree-sitter-fold)

(defvar-local tree-sitter-fold-indicators--timer nil
  "Timer for update indicators.")

(defun tree-sitter-fold-indicators--refresh (buffer &rest _)
  "Refresh indicator overlays to BUFFER."
  (when tree-sitter-fold-indicators-mode
    (tree-sitter-fold-util--with-current-buffer buffer
      (ignore-errors (call-interactively #'origami-open-node))  ; first rebuild tree
      ;; Remove other invalid obsolete overlays
      (let ((ovs (origami-tree-overlays buffer)))
        (dolist (ov (origami-util-overlays-in 'creator 'origami))
          (unless (memq ov ovs) (delete-overlay ov))))
      ;; Remove all indicator overlays
      (remove-overlays (point-min) (point-max) 'creator 'origami-indicators)
      ;; Reapply indicator overlays
      (let ((ovs (overlays-in (point-min) (point-max))) start end tmp-ovs)
        (dolist (ov ovs)
          (when (eq 'origami (overlay-get ov 'creator))
            (setq start (overlay-start ov) end (overlay-end ov)
                  tmp-ovs (overlay-get ov 'ind-ovs))
            (unless (equal start end)
              (when (listp tmp-ovs) (mapc #'delete-overlay tmp-ovs))
              (overlay-put ov 'ind-ovs (origami-indicators--create-overlays start end)))))))))

(defun tree-sitter-fold-indicators--start-timer (&rest _)
  "Start refresh timer."
  (when (timerp tree-sitter-fold-indicators--timer) (cancel-timer tree-sitter-fold-indicators--timer))
  (setq tree-sitter-fold-indicators--timer
        (run-with-idle-timer tree-sitter-fold-indicators-time nil
                             #'tree-sitter-fold-indicators--refresh (current-buffer))))

(defun tree-sitter-fold-indicators--remove-overlays (buffer)
  "Remove all indicators overlays from BUFFER."
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'creator 'origami-indicators)))

(provide 'tree-sitter-fold-indicators)
;;; tree-sitter-fold-indicators.el ends here
