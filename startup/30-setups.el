(eval-after-load 'info
  (require 'info+ nil 'noerror))

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(eval-after-load 'package
  (add-to-list
   'package-archives
   '("marmalade" . "http://marmalade-repo.org/packages/")))
