(require 'cl)
(require 'acme-search)

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

(defun acme-down-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'left)
  (mouse-drag-region click))

(defun acme-mouse-1 (click)
  (interactive "e")
  (setq acme-mouse-state 'none)
  (setq acme-dont-set-region nil)
  (mouse-set-point click))

(defun acme-drag-mouse-1 (click)
  (interactive "e")
  (if (eq acme-dont-set-region nil)
      (mouse-set-region click))
  (setq acme-dont-set-region nil))

(defun acme-down-mouse-2 (click)
  (interactive "e")
  (case acme-mouse-state
    ('left (setq acme-mouse-state 'left-middle)
           (setq acme-dont-set-region t)
           (mouse-kill click))))

(defun acme-mouse-2 (click arg)
  (interactive "e\nP")
  (case acme-mouse-state
    ('left-middle (setq acme-mouse-state 'left))))

(defun acme-down-mouse-3 (click arg)
  (interactive "e\nP")
  (case acme-mouse-state
    ('left (setq acme-mouse-state 'left-right)
           (setq acme-dont-set-region t)
           (yank arg))))

(defun acme-mouse-3 (click)
  (interactive "e")
  (case acme-mouse-state
    ('left-right (setq acme-mouse-state 'left))
    (t (setq acme-mouse-state 'none)
       (acme-search-forward click))))

(provide 'acme-mouse)
