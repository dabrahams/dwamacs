;;
;; This file contains startup code without which I wouldn't be at
;; all lost in a debugging session.
;;

;; Maximize emacs on startup
(ignore-errors
  (require 'maxframe)
  (if (eq system-type 'gnu/linux)
   (setq mf-max-width 1600))  ;; Pixel width of main monitor.
  (add-hook 'window-setup-hook 'maximize-frame t))
