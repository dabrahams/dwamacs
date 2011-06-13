;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dwamacs initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun el-get-subdir
  (&rest d)
  (mapconcat 'file-name-as-directory
             (append
              (list user-emacs-directory "el-get")
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
