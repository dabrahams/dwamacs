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
