;; Undo built-in annoyances
(when window-system
  (global-unset-key [(control ?z)]))

;; Mouse stuff
(define-key me-minor-mode-map [mouse-9] 'mac-mouse-turn-on-fullscreen)
(define-key me-minor-mode-map [drag-mouse-9] 'mac-mouse-turn-on-fullscreen)
(define-key me-minor-mode-map [mouse-11] 'mac-mouse-turn-off-fullscreen)
(define-key me-minor-mode-map [drag-mouse-11] 'mac-mouse-turn-off-fullscreen)

;; Navigation by words
(define-key me-minor-mode-map [(control ?,)] 'backward-word)
(define-key me-minor-mode-map [(control ?.)] 'forward-word)

;; Navigation to other windows (panes)
(define-key me-minor-mode-map "\C-x\C-n" 'other-window)  ; Normally bound to set-goal-column
(define-key me-minor-mode-map [(control return)] 'other-window)

(defun dwa/other-window-backward (&optional n)
  "Select the previous window. Copied from \"Writing Gnu Emacs Extensions\"."
  (interactive "P")
  (other-window (- (or n 1)))
  )

(define-key me-minor-mode-map "\C-x\C-p" 'dwa/other-window-backward) ; Normally bound to mark-page
(define-key me-minor-mode-map [(control shift return)] 'dwa/other-window-backward)

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

;;
;; my-compile, my-recompile - easy compilation with scrolling errors, and easy
;;      recompilation without worrying about what buffer you're in.
;;

;; Used by my-compile and my-recompile to get back to the bottom of a
;; compilation buffer after save-excursion brings us back to the place we
;; started.
(defun my-end-of-current-compilation-buffer()
  (if (equal (buffer-name) "*compilation*")
      (goto-char (point-max))))
  
(defun my-compile(&optional command)
  (interactive)
  (if (interactive-p)
      (call-interactively 'compile)
    (compile command))
  (save-excursion
    (pop-to-buffer "*compilation*")
    (goto-char (point-max)))
  ;; force scrolling despite save-excursion
  (my-end-of-current-compilation-buffer))

(defun my-buffer-exists (buffer)
  "Return t if the buffer exists.
buffer is either a buffer object or a buffer name"
  (bufferp (get-buffer buffer)))
  
(defun my-recompile ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (if (and (my-buffer-exists "*compilation*")
           compile-command)
      (save-excursion
        ;; switching to the compilation buffer here causes the compile command to be
        ;; executed from the same directory it originated from.
        (pop-to-buffer "*compilation*")
        (recompile)
        (pop-to-buffer "*compilation*")
        (goto-char (point-max)))
    ;; else
    (call-interactively 'my-compile))
  ;; force scrolling despite save-excursion
  (my-end-of-current-compilation-buffer))

(add-hook 'compilation-mode-hook (lambda () (visual-line-mode t)))

(define-key me-minor-mode-map [f7] 'my-recompile)
(define-key me-minor-mode-map [(control f7)] 'my-compile)
(define-key me-minor-mode-map [f4] 'next-error)
(define-key me-minor-mode-map [(shift f4)] 'previous-error)
(define-key me-minor-mode-map [(control f4)] 'first-error)

(autoload 'my-wl-check-mail-primary "wl")
(define-key me-minor-mode-map "\C-xM" 'my-wl-check-mail-primary)

;; Org bindings
(eval-when-compile '(require 'gnus-sum))

(defun dwa/org-capture ()
  (interactive)

  ;; Make sure the article buffer is available
  (when (eq major-mode 'gnus-summary-mode)
    (save-window-excursion (gnus-summary-display-article
                            (gnus-summary-article-number))))

  (if (memq major-mode '(gnus-summary-mode gnus-article-mode))
      (with-current-buffer gnus-original-article-buffer
        (nnheader-narrow-to-headers)
        (let ((message-id (message-fetch-field "message-id"))
              (subject (rfc2047-decode-string (message-fetch-field "subject")))
              (from (rfc2047-decode-string (message-fetch-field "from")))
              (date-sent (message-fetch-field "date")))
          (org-capture nil "t")
          (save-excursion
            (insert ?( (replace-regexp-in-string 
                        "\\([^<@]*[^<@ ]\\) *<.*@.*>"
                        "\\1"
                        from) ?) " " 
                    (replace-regexp-in-string
                     "\\[.*? - [A-Za-z]+ #\\([0-9]+\\)\\] (New)"
                     "[[redmine:\\1][#\\1]]"
                     (replace-regexp-in-string "^\\(Re\\|Fwd\\): " ""
                                               subject))))
          (org-set-property "Date" date-sent)
          (org-set-property "Message"
                            (format "[[message://%s][%s]]"
                                    (substring message-id 1 -1)
                                    (subst-char-in-string
                                     ?\[ ?\{ (subst-char-in-string
                                              ?\] ?\} subject))))
          (org-set-property "Submitter" from)))
    (org-capture nil "t")))

(define-key me-minor-mode-map [(meta ?m)] 'dwa/org-capture)

(define-key me-minor-mode-map [(meta ?C)] 'jump-to-org-agenda)

(define-key me-minor-mode-map [(control ?c) ?a] 'org-agenda)

(define-key me-minor-mode-map [(control ?c) (meta ?w)] 'org-store-link)
(define-key me-minor-mode-map [(control ?c) (shift ?w)] 'org-kill-entry)

(define-key me-minor-mode-map [(meta ?{)] 'elscreen-previous)
(define-key me-minor-mode-map [(meta ?})] 'elscreen-next)

;; Unicode
(define-key me-minor-mode-map [(control ?U)] 'unicode-character-shortcut-insert)
(define-key me-minor-mode-map [(control ?\")] 'unicode-smart-double-quote)

(define-key me-minor-mode-map [(meta ?`)] 'other-frame)

(define-key me-minor-mode-map [(control ?h) ?M] 'man)

;;; Need this to make dired-jump work from `C-x C-j'
(require 'dired-x)

(defun dwa/dired-copy-full-path-as-kill ()
  (interactive) 
  (setq current-prefix-arg '(0))
  (call-interactively 'dired-copy-filename-as-kill))

(add-hook 'dired-load-hook 
          (lambda ()
            (define-key dired-mode-map [?W] 'dwa/dired-copy-full-path-as-kill)))

(define-key ctl-x-map [(control ?z)] 'shell-toggle)

(define-key mode-specific-map [?y ?n] 'yas/new-snippet)
(define-key mode-specific-map [?y tab] 'yas/expand)
(define-key mode-specific-map [?y ?f] 'yas/find-snippets)
(define-key mode-specific-map [?y ?r] 'yas/reload-all)
(define-key mode-specific-map [?y ?v] 'yas/visit-snippet-file)

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)
(define-key me-minor-mode-map [(control ?h) ?e] 'lisp-find-map)
(define-key lisp-find-map [?a] 'my-anything-apropos)
(define-key lisp-find-map [?e] 'view-echo-area-messages)
(define-key lisp-find-map [?f] 'find-function)
(define-key lisp-find-map [?d] 'my-describe-symbol)
(define-key lisp-find-map [?i] 'info-apropos)
(define-key lisp-find-map [?k] 'find-function-on-key)
(define-key lisp-find-map [?l] 'find-library)
(define-key lisp-find-map [?o] 'customize-option)
(define-key lisp-find-map [?g] 'customize-group)
(define-key lisp-find-map [?s] 'scratch)
(define-key lisp-find-map [?v] 'find-variable)

;(when (eq system-type 'windows-nt)
;  (add-to-list 'exec-path "C:/msysgit/bin")
;  (add-to-list 'exec-path "C:/msysgit/cmd")

;  (setenv "PATH" (mapconcat (lambda (s) (replace-regexp-in-string "/" "\\\\" s)) exec-path ";")))
