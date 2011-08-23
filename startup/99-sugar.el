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

;; Maximize emacs on startup
(ignore-errors
  (require 'maxframe)
  (if (eq system-type 'gnu/linux)
   (setq mf-max-width 1600))  ;; Pixel width of main monitor.
  (add-hook 'window-setup-hook 'maximize-frame t))
