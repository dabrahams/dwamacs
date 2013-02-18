(eval-after-load 'info
  '(require 'info+ nil 'noerror))

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(eval-after-load 'package
  '(add-to-list
   'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/")))

(eval-after-load 'info-look
  '(info-lookup-add-help
    :mode 'python-mode
    :regexp "[a-zA-Z_0-9.]+"
    :doc-spec
    '(("(python)Python Module Index" )
      ("(python)Index"
       (lambda
         (item)
         (cond
          ((string-match
            "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
           (format "%s.%s" (match-string 2 item) (match-string 1 item)))))))))

(package-initialize)
(require 'session-settings)

;;;_ , gtags

(when nil
(eval-after-load "gtags"
  '(progn
     ;;(diminish 'gtags-mode)

     (require 'anything-gtags)

     (define-key gtags-mode-map "\e," 'anything-gtags-resume)
     (define-key gtags-mode-map "\e." 'gtags-find-tag))))

(add-hook 'c++-mode-hook 'gtags-mode)

(define-key mode-specific-map [?t ?.] 'gtags-find-rtag)
(define-key mode-specific-map [?t ?f] 'gtags-find-file)
(define-key mode-specific-map [?t ?p] 'gtags-parse-file)
(define-key mode-specific-map [?t ?g] 'gtags-find-with-grep)
(define-key mode-specific-map [?t ?i] 'gtags-find-with-idutils)
(define-key mode-specific-map [?t ?s] 'gtags-find-symbol)
(define-key mode-specific-map [?t ?r] 'gtags-find-rtag)
(define-key mode-specific-map [?t ?v] 'gtags-visit-rootdir)
