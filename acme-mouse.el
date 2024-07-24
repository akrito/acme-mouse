;;; acme-mouse.el --- mouse-button chording

;; Author: Alex Kritikos (my gmail.com username is alex.kritikos)
;; Copyright (C) 2009, Alex Kritikos, see section 'Terms'.

;; Description:

;; The Acme editor from Plan 9 has very nice two-button mouse chords
;; for cut, copy and paste, and a handy right-click "find"
;; function. Please see http://swtch.com/plan9port/man/man1/acme.html
;; and http://plan9.bell-labs.com/sys/doc/acme/acme.pdf for more
;; information on how the chords are used. This package aims to work
;; the same as much as makes sense for Emacs.

;; Terms:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Todo:

;; Clicks in the gutter/margin don't seem to work right

;; Sometimes the region is active after the command, when we don't
;; want it to be. Leads to large highlighted regions. Goes away after
;; a click.

;;; Code:

;; Acme mouse chording doesn't make much sense without
;; delete-selection mode
(delete-selection-mode t)

;; Acme doesn't set the selection until you explicitly copy
(setq mouse-drag-copy-region nil)
(setq acme-mouse-state 'none)
(setq acme-last-command 'none)

(defun acme-down-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'left)
  (setq acme-last-command 'none)
  (mouse-set-mark click)
  (mouse-drag-region click))

;; called if mouse doesn't move between button down and up
(defun acme-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq deactivate-mark nil)
  (if (eq acme-last-command 'none)
      (mouse-set-point click))
  (setq transient-mark-mode (cons 'only t))
  (setq acme-last-command 'none))

(defun acme-double-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq deactivate-mark nil)
  ;; if we were just selecting, and not chording, pass to the regular click handler
  (if (eq acme-last-command 'none)
      (progn (mouse-set-point click)
             (acme-select-region)))
  (setq transient-mark-mode (cons 'only t))
  (setq acme-last-command 'none))

;; called if mouse moves between button down and up
(defun acme-drag-mouse-1 (click)
  (interactive "e")
  (if (eq acme-last-command 'none)
      (mouse-set-region click))
  (acme-mouse-1 click))

(defun acme-select-region ()
  (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
    (setq acme-start (car range))
    (setq acme-end (nth 1 range))
    (set-mark acme-start)
    (goto-char acme-end)))

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left)
      (progn
        (setq acme-mouse-state 'left-middle)
        (if (eq acme-last-command 'paste)
            (undo)
          (mouse-set-point click)
          (acme-select-region)
          (kill-region (region-beginning) (region-end)))
        (setq acme-last-command 'cut))
    (if (eq nil (push-button click))
        (unless (mouse-click-in-region-p click)
          (mouse-set-mark click)
          (mouse-drag-region click))
      (push-button click))))

(defun acme-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-middle)
      (setq acme-mouse-state 'left)
    (if (use-region-p)
        (eval-region (region-beginning) (region-end))
      (eval-defun nil))))

(defun acme-drag-mouse-2 (click)
  (interactive "e")
  (if (eq acme-last-command 'none)
      (mouse-set-region click))
  (acme-mouse-2 click))

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (if (eq acme-mouse-state 'left)
      (progn (setq acme-mouse-state 'left-right)
             (cl-case acme-last-command
               (none (mouse-set-point click)
                     (acme-select-region)
                     (delete-region (mark) (point))
                     (yank arg))
               (cut (undo)
                    (set-mark acme-start)
                    (goto-char acme-end)))
             (setq acme-last-command 'paste)
             (setq deactivate-mark nil)
             (activate-mark))
    (unless (mouse-click-in-region-p click)
      (mouse-set-mark click)
      (mouse-drag-region click))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-right)
      (setq acme-mouse-state 'left)
    (if (mouse-click-in-region-p click)
        (acme-search (buffer-substring (region-beginning) (region-end)))
      (acme-search (progn (mouse-set-point click) (thing-at-point 'word))))))

(defun acme-drag-mouse-3 (click)
  (interactive "e")
  (if (eq acme-last-command 'none)
      (mouse-set-region click))
  (acme-mouse-3 click))

(defun mouse-click-in-region-p (click)
  (let ((click-pos (cadr (event-start click))))
    (and (use-region-p)
         (<= click-pos (region-end))
         (>= click-pos (region-beginning)))))

;; Search - modified from Dan McCarthy's acme-search.el

(defun header-line-active-p ()
  (not (null header-line-format)))

(defun move-mouse-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
         (window-coords (window-inside-edges))
         (x (+ (car coords) (car window-coords) -1)) ;the fringe is 0
         (y (+ (cdr coords) (cadr window-coords)
               (if (header-line-active-p)
                   -1
                 0))))
    (set-mouse-position (selected-frame) x y)))

(defun acme-highlight-search (sym)
  "Set the region to the current search result. Assume point is
at the end of the result."
  (set-mark (point))
  (search-backward sym nil t)
  (exchange-point-and-mark))

(defun acme-search (sym)
  "Search forward for the symbol under mouse, moving mouse and point forward.
This is inspired by Rob Pike's Acme."
  (if (file-readable-p sym)
      (find-file-other-window sym)
    (if (search-forward sym nil t)
        (acme-highlight-search sym)
      (let ((saved-point (point)))
        (message "Wrapped search")
        (goto-char (point-min))
        (if (search-forward sym nil t)
            (acme-highlight-search sym)
          (goto-char saved-point))))
    ;;Redisplay the screen if we search off the bottom of the window.
    (unless (posn-at-point)
      (universal-argument)
      (recenter))
    (move-mouse-to-point)))

(define-minor-mode acme-mouse-mode
  "Toggles the Acme mouse mode.
Intended to be an as-close-as-possible reconstruction of the
mouse bindings from the Acme editor from Plan9.

You can enable this mode locally in desired buffers, or use
`global-acme-mouse-mode' to enable it globally."
  :init-value
  nil
  :lighter
  " AMse"
  :keymap
  (list (cons (kbd "<down-mouse-1>") #'acme-down-mouse-1)
        (cons (kbd "<mouse-1>") #'acme-mouse-1)
        (cons (kbd "<drag-mouse-1>") #'acme-drag-mouse-1)
        (cons (kbd "<double-drag-mouse-1>") #'acme-double-mouse-1)
        (cons (kbd "<triple-drag-mouse-1>") #'acme-double-mouse-1)
        (cons (kbd "<double-mouse-1>") #'acme-double-mouse-1)
        (cons (kbd "<triple-mouse-1>") #'acme-double-mouse-1)
        (cons (kbd "<down-mouse-2>") #'acme-down-mouse-2)
        (cons (kbd "<mouse-2>") #'acme-mouse-2)
        (cons (kbd "<drag-mouse-2>") #'acme-drag-mouse-2)
        (cons (kbd "<down-mouse-3>") #'acme-down-mouse-3)
        (cons (kbd "<mouse-3>") #'acme-mouse-3)
        (cons (kbd "<drag-mouse-3>") #'acme-drag-mouse-3)))

(defun turn-on-acme-mouse-mode ()
  (when (not acme-mouse-mode) (acme-mouse-mode 1)))

(define-globalized-minor-mode global-acme-mouse-mode
  acme-mouse-mode
  turn-on-acme-mouse-mode)

(provide 'acme-mouse)
