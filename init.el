;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dwamacs initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'find-func)
;;(let ((cedet-lisp (file-name-directory (find-library-name "cedet"))))
;;  (setq load-path (delete cedet-lisp (delete (directory-file-name cedet-lisp) load-path))))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(if (require 'el-get nil t)
    (el-get 'sync)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(el-get 'sync)

(defun el-get-subdir
  (&rest d)
  (mapconcat 'file-name-as-directory
             (append
              (list (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d/") "el-get")
              d)
             ""))
(setq elhome-directory (el-get-subdir "dwamacs")
      user-site-lisp-directory (expand-file-name "site-lisp" elhome-directory)
      ;; If I don't set this during startup, an error can cause it to
      ;; be set to something that invokes TRAMP whenever I try to
      ;; switch buffers.
      default-directory "~"
      bcc-enabled nil
      redisplay-dont-pause t) ;; See [[message://83ipovw2z2.fsf@gnu.org]]
(load
 (concat
  (el-get-subdir "elhome")
  "elhome"))
(elhome-init)
(put 'narrow-to-region 'disabled nil)
