;; Navigation by words
(define-key me-minor-mode-map [(control ?,)] 'backward-word)
(define-key me-minor-mode-map [(control ?.)] 'forward-word)

;; Navigation to other windows (panes)
(define-key me-minor-mode-map "\C-x\C-n" 'other-window)  ; Normally bound to set-goal-column

(defun dwa/other-window-backward (&optional n)
  "Select the previous window. Copied from \"Writing Gnu Emacs Extensions\"."
  (interactive "P")
  (other-window (- (or n 1)))
  )

(define-key me-minor-mode-map "\C-x\C-p" 'dwa/other-window-backward) ; Normally bound to mark-page

;; This is the way I like it; the defaults go to the beginning and/or end of line
(define-key me-minor-mode-map [home] 'beginning-of-buffer)
(define-key me-minor-mode-map [end] 'end-of-buffer)

(defun dwa/match-paren (arg)
  (interactive "P")
  (if arg
      () ;;(insert "%")  ; insert the character we're bound to
    (cond ((looking-at "[[({]")
           (forward-sexp 1)
           (forward-char -1))
          ((looking-at "[]})]")
           (forward-char 1)
           (forward-sexp -1))
          (t
           ;; (insert "%")  ; insert the character we're bound to
      ))))

(define-key me-minor-mode-map [( control ?\( )] 'dwa/match-paren)

(defun dwa/other-buffer ()
  "Switch to the most recently visited buffer without asking"
  (interactive)
  (switch-to-buffer nil))

;; This is normally set to bring up a buffer list, but there are many other
;; ways to do this seldom-desired function (e.g. C-mouse1, or look at the
;; "Buffers" menu at the top of the frame).
(define-key me-minor-mode-map "\C-x\C-b" 'dwa/other-buffer)

;; Miscellaneous
(define-key me-minor-mode-map "\C-x\C-g" 'goto-line)

(defun dwa/kill-current-buffer ()
  "Kill the current buffer without asking, unless it's modified file, in which case ask first"
  (interactive)
  (kill-buffer (current-buffer)))

(define-key me-minor-mode-map "\C-x\C-k" 'dwa/kill-current-buffer)
(define-key me-minor-mode-map "\C-xg" 'magit-status)
