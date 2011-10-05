;; Support initial tabs in Python backtraces, as produced by Buildbot.
(let* ((clause (assoc 'caml compilation-error-regexp-alist-alist))
       (pat (cadr clause)))
  (when (string= (substring pat 0 2) "^ ")
    (setcdr clause
            (cons
             (concat "^\\s-" (substring pat 2))
             (cddr clause)))))

(if (featurep 'compile-)
    (require 'compile+)
  (warn "compile- must be loaded before compile and compile+"))

(add-hook 'compilation-mode-hook
          (lambda () (make-local-variable 'hl-line-sticky-flag)
            (setq hl-line-sticky-flag t)
            (hl-line-mode)
            ))

(add-hook 'next-error-hook
          (lambda () 
            (with-current-buffer next-error-last-buffer
              (make-local-variable 'hl-line-sticky-flag)
              (setq hl-line-sticky-flag t)
              (hl-line-mode nil)
              (hl-line-mode t))))
