;;
;; This file contains startup code without which I wouldn't be at
;; all lost in a debugging session.
;;

(ignore-errors (server-mode))

(prefer-coding-system 'utf-8)

;; auto modes
(add-to-list 'auto-mode-alist
             '("\\.\\(text\\|md\\|mkdn\\|mmd\\|markdown\\)\\'" . markdown-mode))

(defun request-feature (feature)
  (or (require feature nil 'noerror)
      (and (message "requested feature %s not available" feature) nil)))

(when (request-feature 'workgroups)
  (workgroups-mode 1)
  (if (bound-and-true-p wg-prefixed-map)
      (define-key wg-prefixed-map [(control ?z)] 'wg-switch-to-previous-workgroup)
    (if (file-readable-p "~/.emacs.d/workgroups")
        (wg-load "~/.emacs.d/workgroups")))

  (when nil
  (defadvice ido-visit-buffer (before switch-to-buffers-workgroup
                                      (buffer method &optional record)
                                      activate)
    "If a workgroup is showing BUFFER, switch to it first"
    (wg-aif (wg-find-buffer (if (bufferp buffer)
                                (buffer-name buffer)
                              buffer))
        (ignore-errors
          (wg-switch-to-workgroup it))))))


;; Keep some very persistent modes out of the mode line display
(when (request-feature 'diminish)
  (dolist (m '(workgroups me-minor))
    (diminish (intern (concat (symbol-name m) "-mode")))))

(defface dwa/glasses
  '((t :weight bold :underline t))
  "Face for highlighting capital letters in Camel-case")

;;;;;;;;;;
;; FFAP
;;;;;;;;;;

;; Automatically find files and URLs at point
(ffap-bindings)

;; These hooks set up by ffap-bindings rebind `M-m', which I use for
;; org capture
(remove-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
(remove-hook 'gnus-article-mode-hook 'ffap-gnus-hook)

(when (request-feature 'ws-trim)
;  (global-ws-trim-mode t)
  (set-default 'ws-trim-level 0)
  (setq ws-trim-global-modes '(guess (not message-mode eshell-mode))))

;; Pretty ^L
(when (request-feature 'pp-c-l)
  (pretty-control-l-mode))

;; Page navigation 
(request-feature 'page-ext)

;; Per-window point
(when (request-feature 'per-window-point)
  (pwp-mode))

;;;;;;;;

(when (request-feature 'session)
  (add-hook 'after-init-hook 'session-initialize))

;;;;;;;

(when (request-feature 'color-theme)
  (define-color-theme
    dwa-dark-theme
    "Dave Abrahams"
    "Refinement of zenburn for dark spaces"
    (color-theme-zenburn)
    (load-theme 'zenburn-overrides))
  (color-theme-initialize)
  (request-feature 'org-faces)
  ;; Store away the emacs default theme so I can get back there
  (fset 'color-theme-emacs-default (color-theme-make-snapshot))
  (add-to-list 'color-themes
               '(color-theme-emacs-default "Emacs Default" "Gnu"))
  (add-hook 'after-init-hook 'color-theme-select :append)
;  (add-hook 'after-init-hook 'dwa-dark-theme :append)
  )

(defun dark ()
  (interactive)
  (load-theme 'zenburn)
  (load-theme 'zenburn-overrides))

(defun light ()
  (interactive)
  (when (> emacs-major-version 23)
    ;; There's a bug in emacs 23 that makes this lose mode line faces
    (disable-theme 'zenburn-overrides))
  (disable-theme 'zenburn))

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

;(request-feature 'elscreen-buffer-list)

;(request-feature 'frame-bufs)
;(ignore-errors (frame-bufs-mode t))

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

(defun ed () 
  (interactive)
  (edebug-defun))

(defun de ()
  (interactive)
  (end-of-defun)
  (backward-sexp)
  (forward-sexp)
  (eval-last-sexp nil))



;;
;; Avoid putting new stuff in tiny windows, per
;; [[message://4ED8FAB5.3050306@gmx.at]]
;;
(if (not (string-lessp emacs-version "24"))
 (defun make-small-windows-softly-dedicated ()
  (walk-window-tree
   (lambda (window)
     (cond
      ((and (> (window-total-size window) 10)
	    (eq (window-dedicated-p window) 'too-small))
       (set-window-dedicated-p window nil))
      ((and (<= (window-total-size window) 10)
	    (not (window-dedicated-p window)))
       (set-window-dedicated-p window 'too-small))))))
 ;; more expensive version for Emacs23 since constructing the window
 ;; list means one cons cell for each live window on the changed frame
 ;; [[message://4EF4AD73.6030006@gmx.at]]
 (defun make-small-windows-softly-dedicated ()
   (dolist (window (window-list nil 'nomini))
     (cond
      ((and (> (window-height window) 10)
            (eq (window-dedicated-p window) 'too-small))
       (set-window-dedicated-p window nil))
      ((and (<= (window-height window) 10)
            (not (window-dedicated-p window)))
       (set-window-dedicated-p window 'too-small))))))

(add-hook 'window-configuration-change-hook 'make-small-windows-softly-dedicated)

  
;; Makes `C-c RET C-a' send the current file as an attachment in dired
;; [[message://m2vcukdcsu.fsf@gmail.com]]
(autoload 'gnus-dired-mode "gnus-dired" nil t)
(add-hook 'dired-mode-hook 'gnus-dired-mode)

;; Maximize emacs on startup
(ignore-errors
  (require 'maxframe)
  (add-hook 'window-setup-hook 'maximize-frame t))
