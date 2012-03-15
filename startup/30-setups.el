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

