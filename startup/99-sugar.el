;;
;; This file contains startup code without which I wouldn't be at
;; all lost in a debugging session.
;;

(prefer-coding-system 'utf-8)

;; auto modes
(add-to-list 'auto-mode-alist
             '("\\.\\(text\\|mkdn\\|mmd\\|markdown\\)\\'" . markdown-mode))

;;;;;;;;;;
;; FFAP
;;;;;;;;;;

;; Automatically find files and URLs at point
(ffap-bindings)

;; These hooks set up by ffap-bindings rebind `M-m', which I use for
;; org capture
(remove-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
(remove-hook 'gnus-article-mode-hook 'ffap-gnus-hook)

(when (require 'ws-trim nil 'noerror)
  (global-ws-trim-mode t)
  (set-default 'ws-trim-level 1)
  (setq ws-trim-global-modes '(guess (not message-mode eshell-mode))))

;; Pretty ^L
(ignore-errors 
  (require 'pp-c-l)
  (pretty-control-l-mode))

;; Page navigation 
(require 'page-ext)

;; Per-window point
(ignore-errors
  (require 'per-window-point)
  (pwp-mode))

;;;;;;;;

(ignore-errors
  (require 'session)
  (add-hook 'after-init-hook 'session-initialize))

;;;;;;;

(when (ignore-errors (progn (zenburn) t))
  (load-theme 'zenburn-overrides))

(setq frame-title-format
    '(:eval
      (concat "%["
              (if buffer-file-name default-directory "%b")
              " - %I%]")))

;; Make sure any customizations are saved before exiting.  Should
;; eventually be replaced by the use of cus-edit+
(defun dwa/save-customizations-before-exit ()
  (let ((doe (get 'debug-on-error 'customized-value))
        (doq (get 'debug-on-quit 'customized-value)))
    (unwind-protect
        (progn
          (put 'debug-on-error 'customized-value nil)
          (put 'debug-on-quit 'customized-value nil)
          (condition-case err 
              (progn (customize-unsaved) nil)
            (error 
             (or (equal err '(error "No user options are set but unsaved"))
                 (signal (car err) (cdr err))))))
      (put 'debug-on-error 'customized-value doe)
      (put 'debug-on-quit 'customized-value doq))))
(add-to-list 'kill-emacs-query-functions 'dwa/save-customizations-before-exit)

;(require 'elscreen-buffer-list nil 'noerror)

(require 'frame-bufs nil 'noerror)
(ignore-errors (frame-bufs-mode t))

;; Man pages often come out too wide
(defadvice Man-getpage-in-background
  (around Man-narrow-please activate compile preactivate)
  (let ((Man-width (when (> (window-width) 90) 90)))
    ad-do-it))
(setenv "MANWIDTH" "") ;; the `man' function respects these environment variables
(setenv "COLUMNS" "")  ;; so let's dis' them.

;; The git pager sux except in a terminal
(setenv "GIT_PAGER" "")

(defun tde () 
  (interactive)
  (call-interactively 'toggle-debug-on-error))

(defun tdq () 
  (interactive)
  (call-interactively 'toggle-debug-on-quit))

;; Makes `C-c RET C-a' send the current file as an attachment in dired
;; [[message://m2vcukdcsu.fsf@gmail.com]]
(autoload 'gnus-dired-mode "gnus-dired" nil t)
(add-hook 'dired-mode-hook 'gnus-dired-mode)

;; Maximize emacs on startup
(add-hook 'window-setup-hook 
          (lambda () (modify-frame-parameters nil '((fullscreen . maximized))))
          t)
