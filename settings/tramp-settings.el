;; (require 'tramp-cmds)
;; (defadvice tramp-cleanup-all-buffers (after tramp-reset-default-directories activate)
;;   (dolist (b (buffer-list))
;;     (with-current-buffer b
;;       (when (tramp-tramp-file-p default-directory)
;;         (setq default-directory user-emacs-directory)
;;         ))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tramp-backup-directory-alist
   (quote
    (("." . "~/.emacs.d/backups"))))
 '(tramp-default-host "localhost")
 '(tramp-default-proxies-alist
   (quote
    (("\\`localhost\\'" nil nil)
     ("\\`206.217.198.21\\'" nil nil)
     ("\\`.+\\'" "\\`root\\'" "/ssh:%h:")))
   nil nil "
Gets around the common setting that prohibits ssh login as root.

Don't do any proxying for connections to localhost (depends
on the customization of tramp-default-host to \"localhost\" for simple
matching), and otherwise, if sudo'ing somewhere, ssh there first and
then sudo on the remote host itself.")
 '(tramp-remote-path
   (quote
    (tramp-default-remote-path "/usr/sbin" "/usr/local/sbin" "/usr/local/bin" "/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"))))
