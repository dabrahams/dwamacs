(add-to-list 'load-path (expand-file-name (elhome-path-join user-emacs-directory "el-get" "el-get")))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(require 'el-get)

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/dwamacs/el-get-recipes/personal/")
