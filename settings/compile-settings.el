;; Support initial tabs in Python backtraces, as produced by Buildbot.
(let* ((clause (assoc 'caml compilation-error-regexp-alist-alist))
       (pat (cadr clause)))
  (when (and clause (string= (substring pat 0 2) "^ "))
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
            (hl-line-mode t)
            ))

(add-hook 'next-error-hook
          (lambda () 
            (with-current-buffer next-error-last-buffer
              (make-local-variable 'hl-line-sticky-flag)
              (setq hl-line-sticky-flag t)
              (hl-line-mode nil)
              (hl-line-mode t))))

(defun cmake-project-filename ()
  (let ((filename (match-string-no-properties 1)))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents-literally "cmake_install.cmake")
        (goto-char (point-min))
        (re-search-forward "Install script for directory: \\(.+\\)")
        (cons filename (match-string-no-properties 1))))))

(push 'cmake compilation-error-regexp-alist)
(push '(cmake "^\\(?:CMake Error at \\|  \\)\\(.+?\\):\\([0-9]+\\) ([A-Za-z_][A-Za-z0-9_]*)"
              (cmake-project-filename) 2)
      compilation-error-regexp-alist-alist)
