;;
;; This file contains startup code without which I wouldn't be at
;; all lost in a debugging session.
;;

;;;;;;;;;;
;; FFAP
;;;;;;;;;;

;; Automatically find files and URLs at point
(ffap-bindings)

;; These hooks set up by ffap-bindings rebind `M-m', which I use for
;; org capture
(remove-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
(remove-hook 'gnus-article-mode-hook 'ffap-gnus-hook)

;;;;;;;;

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;;;;;;

(ignore-errors
  (zenburn))

(setq frame-title-format
    '(:eval
      (concat "%["
              (if buffer-file-name default-directory "%b")
              " - %I%]")))

;; Make sure any customizations are saved before exiting.  Should
;; eventually be replaced by the use of cus-edit+
(defun dwa/save-customizations-before-exit ()
  (condition-case err 
      (progn (customize-unsaved) nil)
    (error 
     (or (equal err '(error "No user options are set but unsaved"))
         (apply 'signal err)))))
(add-to-list 'kill-emacs-query-functions 'dwa/save-customizations-before-exit)

(ignore-errors (require 'elscreen))

;; The git pager sux except in a terminal
(setenv "GIT_PAGER" "")

;; Makes `C-c RET C-a' send the current file as an attachment in dired
;; [[message://m2vcukdcsu.fsf@gmail.com]]
(autoload 'gnus-dired-mode "gnus-dired" nil t)
(add-hook 'dired-mode-hook 'gnus-dired-mode)

;; Maximize emacs on startup
(ignore-errors
  (require 'maxframe)
  (if (eq system-type 'gnu/linux)
   (setq mf-max-width 1600))  ;; Pixel width of main monitor.
  (add-hook 'window-setup-hook 'maximize-frame t))
