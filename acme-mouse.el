;;; acme-mouse.el --- mouse-button chording

;; Original Author: Alex Kritikos (my gmail.com username is alex.kritikos)
;; Copyright (C) 2009, Alex Kritikos, see section 'Terms'.

;; Description:

;; The Acme editor from Plan 9 has very nice two-button mouse chords for cut,
;; copy and paste, and a handy right-click "find" function. Please see
;; https://swtch.com/plan9port/man/man1/acme.html for more information on how
;; the chords are used. This package aims to work the same as much as makes
;; sense for Emacs.

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

;; Clicks in the gutter/margin don't seem to work right.  They are registered,
;; but if you force the level of `mouse-selection-click-count', it doesn't go to
;; the desired level (2) until you release mouse-1, breaking chording.
;; Currently using the normal triple-click to select a line and be able to
;; chord.

;;; Code:

;; MOUSE-1

(defun acme-down-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'left
	acme-last-command 'none)
  (mouse-set-mark click)
  (mouse-drag-region click))

;; called if mouse doesn't move between button down and up

(defun acme-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none
	deactivate-mark nil)
  (if (eq acme-last-command 'none)
      (mouse-set-point click))
  (acme-mouse--end-click))

(defun acme-double-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none
	deactivate-mark nil)
  (when (eq acme-last-command 'none) ; if just selecting, and not chording, pass to the regular click handler
    (mouse-set-point click)
    (acme-mouse--select-region))
  (acme-mouse--end-click))

;; MOUSE-2

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left)
      (progn
	(setq acme-mouse-state 'left-middle)
	(if (eq acme-last-command 'paste)
	    (undo)
	  (mouse-set-point click)
	  (acme-mouse--select-region)
	  (kill-region (region-beginning) (region-end)))
	(setq acme-last-command 'cut))
    (if (eq nil (push-button click))
	(unless (acme-mouse--click-in-region-p click)
	  (mouse-set-mark click)
	  (mouse-drag-region click))
      (push-button click))))

(defun acme-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-middle)
      (setq acme-mouse-state 'left)
    (if (use-region-p)
	(eval-region (region-beginning) (region-end) t)
      (eval-defun nil))))

;; MOUSE-3

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (if (eq acme-mouse-state 'left)
      (progn (setq acme-mouse-state 'left-right)
	     (cl-case acme-last-command
	       (none (mouse-set-point click)
		     (acme-mouse--select-region)
		     (delete-region (mark) (point))
		     (yank arg))
	       (cut (undo)
		    (set-mark acme-start)
		    (goto-char acme-end)))
	     (setq acme-last-command 'paste)
	     (setq deactivate-mark nil)
	     (activate-mark)
	     (acme-mouse--show-region))
    (unless (acme-mouse--click-in-region-p click)
      (mouse-set-mark click)
      (mouse-drag-region click))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-right)
      (setq acme-mouse-state 'left)
    (if (acme-mouse--click-in-region-p click)
	(acme-mouse--search (buffer-substring (region-beginning) (region-end)))
      (acme-mouse--search (progn (mouse-set-point click) (thing-at-point 'word))))
    (acme-mouse--show-region)))

;; INTERNAL/HELPERS

(defmacro acme-mouse-dragger (func)
  `(lambda (click)
     (interactive "e")
     (if (eq acme-last-command 'none)
	 (mouse-set-region click))
     (,func click)))

(defun acme-mouse--show-region ()
  ;; Show the region a click, chord, or search left in temporary Transient
  ;; Mark mode, as `mouse-set-region-1' does. Wrap only an active mark and
  ;; only once, so no wrap lingers unwound and none nests.
  (when (and mark-active
	     (not (eq (car-safe transient-mark-mode) 'only)))
    (setq-local transient-mark-mode
		(cons 'only (unless (eq transient-mark-mode 'lambda)
			      transient-mark-mode)))))

(defun acme-mouse--end-click ()
  (acme-mouse--show-region)
  (setq acme-last-command 'none))

(defun acme-mouse--select-region ()
  (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
    (setq acme-start (car range)
	  acme-end (nth 1 range))
    (set-mark acme-start)
    (goto-char acme-end)))

(defun acme-mouse--click-in-region-p (click)
  (let ((click-pos (cadr (event-start click))))
    (and (use-region-p)
	 (<= click-pos (region-end))
	 (>= click-pos (region-beginning)))))

(defun acme-mouse--move-to-point ()
  "Move the mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
	 (window-coords (window-inside-edges))
	 (x (+ (car coords) (car window-coords) -1)) ;the fringe is 0
	 (y (+ (cdr coords) (cadr window-coords)
	       (if (not (null header-line-format))
		   -1
		 0))))
    (set-mouse-position (selected-frame) x y)))

(defun acme-mouse--highlight-search (sym)
  "Set the region to the current search result. Assume point is
at the end of the result."
  (set-mark (point))
  (search-backward sym nil t)
  (exchange-point-and-mark))

;; Search - modified from Dan McCarthy's acme-search.el

(defun acme-mouse--search (sym)
  "Search forward for the symbol under mouse, moving mouse and point forward.
This is inspired by Rob Pike's Acme."
  (if (file-readable-p sym)
      (find-file-other-window sym)
    (if (search-forward sym nil t)
	(acme-mouse--highlight-search sym)
      (let ((saved-point (point)))
	(message "Wrapped search")
	(goto-char (point-min))
	(if (search-forward sym nil t)
	    (acme-mouse--highlight-search sym)
	  (goto-char saved-point))))
    (unless (posn-at-point) ; Redisplay the screen if we search off the bottom of the window.
      (universal-argument)
      (recenter))
    (acme-mouse--move-to-point)))

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
	(cons (kbd "<drag-mouse-1>") (acme-mouse-dragger acme-mouse-1))
	(cons (kbd "<double-drag-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<triple-drag-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<double-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<triple-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<down-mouse-2>") #'acme-down-mouse-2)
	(cons (kbd "<mouse-2>") #'acme-mouse-2)
	(cons (kbd "<drag-mouse-2>") (acme-mouse-dragger acme-mouse-2))
	(cons (kbd "<down-mouse-3>") #'acme-down-mouse-3)
	(cons (kbd "<mouse-3>") #'acme-mouse-3)
	(cons (kbd "<drag-mouse-3>") (acme-mouse-dragger acme-mouse-3))
	(cons (kbd "<left-fringe> <down-mouse-1>") #'acme-down-mouse-1)
	(cons (kbd "<left-fringe> <mouse-1>") #'acme-mouse-1)
	(cons (kbd "<left-fringe> <double-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<left-fringe> <down-mouse-2>") #'acme-down-mouse-2)
	(cons (kbd "<left-fringe> <mouse-2>") #'acme-mouse-2)
	(cons (kbd "<left-fringe> <down-mouse-3>") #'acme-down-mouse-3)
	(cons (kbd "<left-fringe> <mouse-3>") #'acme-mouse-3)
	(cons (kbd "<right-fringe> <down-mouse-1>") #'acme-down-mouse-1)
	(cons (kbd "<right-fringe> <mouse-1>") #'acme-mouse-1)
	(cons (kbd "<right-fringe> <double-mouse-1>") #'acme-double-mouse-1)
	(cons (kbd "<right-fringe> <down-mouse-2>") #'acme-down-mouse-2)
	(cons (kbd "<right-fringe> <mouse-2>") #'acme-mouse-2)
	(cons (kbd "<right-fringe> <down-mouse-3>") #'acme-down-mouse-3)
	(cons (kbd "<right-fringe> <mouse-3>") #'acme-mouse-3))
  :after-hook
  (lambda ()
    (delete-selection-mode t)
    (setq mouse-drag-copy-region nil
	  acme-mouse-state 'none
	  acme-last-command 'none)))

(defun turn-on-acme-mouse-mode ()
  (when (not acme-mouse-mode) (acme-mouse-mode 1)))

(define-globalized-minor-mode global-acme-mouse-mode
  acme-mouse-mode
  turn-on-acme-mouse-mode)

(provide 'acme-mouse)
;;; acme-mouse.el ends here
