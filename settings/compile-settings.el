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
  (let ((filename (match-string-no-properties 2)))
    (if (file-name-absolute-p filename) filename
      (save-match-data
        (with-temp-buffer
          (insert-file-contents-literally "cmake_install.cmake")
          (goto-char (point-min))
          (re-search-forward "Install script for directory: \\(.+\\)")
          (cons filename (match-string-no-properties 1))))
      )
    ))

(push 'cmake compilation-error-regexp-alist)
(push '(cmake "^\\(?:CMake \\(?:Error\\|Warnin\\(g\\)\\) at \\|  \\)\\(.+?\\):\\([0-9]+\\) ([A-Za-z_][A-Za-z0-9_]*)"
              (cmake-project-filename) 3 nil (1))
      compilation-error-regexp-alist-alist)

(push 'mac-assert compilation-error-regexp-alist)
(push `(mac-assert "^Assertion failed: \\(.+?\\), function .+?, file \\(.+?\\), line \\([0-9]+\\)\\.$"
              2 3 ,(not :column) ,(not :just-a-warning) 1)
      compilation-error-regexp-alist-alist)


(push 'mygnu compilation-error-regexp-alist)
(push '(mygnu
     ;; The first line matches the program name for

     ;;     PROGRAM:SOURCE-FILE-NAME:LINENO: MESSAGE

     ;; format, which is used for non-interactive programs other than
     ;; compilers (e.g. the "jade:" entry in compilation.txt).

     ;; This first line makes things ambiguous with output such as
     ;; "foo:344:50:blabla" since the "foo" part can match this first
     ;; line (in which case the file name as "344").  To avoid this,
     ;; the second line disallows filenames exclusively composed of
     ;; digits.

     ;; Similarly, we get lots of false positives with messages including
     ;; times of the form "HH:MM:SS" where MM is taken as a line number, so
     ;; the last line tries to rule out message where the info after the
     ;; line number starts with "SS".  --Stef

     ;; The core of the regexp is the one with *?.  It says that a file name
     ;; can be composed of any non-newline char, but it also rules out some
     ;; valid but unlikely cases, such as a trailing space or a space
     ;; followed by a -, or a colon followed by a space.

     ;; The "in \\|from " exception was added to handle messages from Ruby.
     "^\\(?:[[:alpha:]][-[:alnum:].]+: ?\\|[ \t]+\\(?:in \\|from \\)\\|\\(?:.+?\\[\\[[^@]*@\\)\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^/\n]\\|:[^ \n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:[.:]\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\.\\([0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
 *[Ee]rror\\|\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
     1 (2 . 4) (3 . 5) (6 . 7))
      compilation-error-regexp-alist-alist)
