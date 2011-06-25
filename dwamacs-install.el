(setq elhome-directory 
      (mapconcat 'file-name-as-directory 
                 (list user-emacs-directory "el-get" "dwamacs") ""))

(setq debug-on-error t)

(eval-after-load 'elhome
  '(progn
     (add-to-list 'el-get-sources
           '(:name dwamacs
                    :type git
                    :url "git://github.com/dabrahams/dwamacs.git"
                    :depends (elhome)
                    :post-init (lambda () (elhome-init))))
     (message "*** INSTALLING DWAMACS ***")
     (el-get-install "dwamacs")))

(unless (require 'elhome nil t)
  (url-retrieve
   "http://github.com/dabrahams/elhome/raw/master/elhome-install.el"
   (lambda (s) 
     (end-of-buffer)
     (eval-print-last-sexp))))
