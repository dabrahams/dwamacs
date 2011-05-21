;; Support initial tabs in Python backtraces, as produced by Buildbot.
(let* ((clause (assoc 'caml compilation-error-regexp-alist-alist))
       (pat (cadr clause)))
  (when (string= (substring pat 0 2) "^ ")
    (setcdr clause
            (cons
             (concat "^\\s-" (substring pat 2))
             (cddr clause)))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
