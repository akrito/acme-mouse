(require 'cl)

;; Implements Acme-style mouse chording in Emacs
;; Copyright 2009 Alex Kritikos

;; TODO
;; Currently acme-search acts like * in vim. We should rewrite it to
;; act more like Acme:
;;   If there's a region, match against it
;;   If not, match against the word under the cursor
;;   If a file matches the text, open or switch to it in a new window
;;   Else, search through the file, wrapping at the bottom
;;   Allow drag-highlighting with the right button (secondary selection?)
;;
;; Reset the modification status when yank follows kill or kill follows yank
;;
;; Once a chord is pressed, don't let the mouse figure into region
;; calculations. Consider them frozen. It's like there's a selection
;; mode, then a command mode.

(require 'acme-search)

;; Acme mouse chording doesn't make much sense without
;; delete-selection mode
(delete-selection-mode t)

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
  (setq acme-cut nil)
  (setq acme-pasted nil)
  ;; pass to regular click handler
  (mouse-drag-region click))

;; called if mouse doesn't move between button down and up
(defun acme-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq deactivate-mark nil)
  (mouse-set-point click))

;; called if mouse moves between button down and up
(defun acme-drag-mouse-1 (click)
  (interactive "e")
  (if (eq acme-cut nil)
      (mouse-set-region click))
  (if acme-pasted
      (progn (setq deactivate-mark nil)
             (exchange-point-and-mark))))

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left)
      (progn (setq acme-mouse-state 'left-middle)
             (setq acme-cut t)
             (mouse-set-point click)
             (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
               (set-mark (car range))
               (goto-char (nth 1 range))
               (kill-region (mark) (point))))))

(defun acme-mouse-2 ()
  (interactive)
  (if (eq acme-mouse-state 'left-middle)
      (setq acme-mouse-state 'left)))

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (if (eq acme-mouse-state 'left)
      (progn
        (setq acme-mouse-state 'left-right)
        (if (eq acme-cut nil)
            (progn (setq acme-pasted t)
                   (mouse-set-point click)
                   (let ((range (mouse-start-end (mark) (point) mouse-selection-click-count)))
                     (set-mark (car range))
                     (goto-char (nth 1 range))
                     (delete-region (mark) (point)))
                   (yank arg))
          (undo))
        (setq deactivate-mark nil)
        (exchange-point-and-mark))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (if (eq acme-mouse-state 'left-right)
      (setq acme-mouse-state 'left)
    (acme-search-forward click)))

(provide 'acme-mouse)
