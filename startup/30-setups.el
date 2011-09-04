(eval-after-load 'info
  (require 'info+ nil 'noerror))

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
