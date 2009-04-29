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

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Todo:

;; Currently acme-search acts like * in vim. We should rewrite it to
;; act more like Acme:
;;   If there's a region, match against it
;;   If not, match against the word under the cursor
;;   If a file matches the text, open or switch to it in a new window
;;   Else, search through the file, wrapping at the bottom
;;   Allow drag-highlighting with the right button (secondary selection?)

;; Make this a proper minor mode without clobbering keymaps and global
;; settings

;; Clicks in the gutter/margin don't seem to work right

;; Sometimes the region is active after the command, when we don't
;; want it to be. Leads to large highlighted regions

;; Cutting or Pasting, then moving the arrow keys extends the
;; region. Moving the arrow keys should remove the region

(require 'cl)
(require 'acme-search)

;; Acme mouse chording doesn't make much sense without
;; delete-selection mode
(delete-selection-mode t)
;; Acme doesn't set the selection until you explicitly copy
(setq mouse-drag-copy-region nil)

;; default: mouse-drag-region
(global-set-key [(down-mouse-1)] 'acme-down-mouse-1)

;; default: mouse-set-point
(global-set-key [(mouse-1)] 'acme-mouse-1)

;; default: mouse-set-region
(global-set-key [(drag-mouse-1)] 'acme-drag-mouse-1)

;; default: none
(global-set-key [(down-mouse-2)] 'acme-down-mouse-2)

;; default: mouse-yank-at-click
(global-set-key [(mouse-2)] 'acme-mouse-2)

;; default: none
(global-set-key [(down-mouse-3)] 'acme-down-mouse-3)

;; default: mouse-save-then-kill
(global-set-key [(mouse-3)] 'acme-mouse-3)

;; everything starts with this function, so set state accordingly
(defun acme-down-mouse-1 (click)
  (interactive "e")
  ;; which buttons are currently pressed?
  (setq acme-mouse-state 'left)
  (mouse-set-mark click)
  (setq acme-last-command 'none)
  ;; pass to regular click handler
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

;; called if mouse moves between button down and up
(defun acme-drag-mouse-1 (click)
  (interactive "e")
  (if (eq acme-last-command 'none)
      (mouse-set-region click)
    (setq deactivate-mark nil)
    (exchange-point-and-mark))
  (setq transient-mark-mode (cons 'only t))
  (setq acme-last-command 'none))

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left)
      (progn (setq acme-mouse-state 'left-middle)
             (if (eq acme-last-command 'paste)
                 (undo)
               (mouse-set-point click)
               (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
                 (setq acme-start (car range))
                 (setq acme-end (nth 1 range))
                 (set-mark acme-start)
                 (goto-char acme-end)
                 (kill-region (mark) (point))))
             (setq acme-last-command 'cut))))

(defun acme-mouse-2 ()
  (interactive)
  (if (eq acme-mouse-state 'left-middle)
      (setq acme-mouse-state 'left)))

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (if (eq acme-mouse-state 'left)
      (progn
        (setq acme-mouse-state 'left-right)
        (case acme-last-command
          ('none (mouse-set-point click)
                 (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
                   (setq acme-start (car range))
                   (setq acme-end (nth 1 range))
                   (set-mark acme-start)
                   (goto-char acme-end)
                   (delete-region (mark) (point))
                   (yank arg)))
          ('cut (undo)
                (set-mark acme-start)
                (goto-char acme-end)))
        (setq acme-last-command 'paste)
        (setq deactivate-mark nil)
        (activate-mark))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-right)
      (setq acme-mouse-state 'left)
    (acme-search-forward click)))

(provide 'acme-mouse)
