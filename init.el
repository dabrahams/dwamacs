;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dwamacs initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun el-get-subdir
  (&rest d)
  (mapconcat 'file-name-as-directory
             (append
              (list (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d/") "el-get")
              d)
             ""))
(setq elhome-directory
      (el-get-subdir "dwamacs"))
(load
 (concat
  (el-get-subdir "elhome")
  "elhome"))
(elhome-init)
(put 'narrow-to-region 'disabled nil)
