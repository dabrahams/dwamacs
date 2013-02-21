;; This file makes sure that Drew Adams' "compile-" library is loaded
;; before the built-in "compile" library

;; load compile-, but first make sure it doesn't stomp on zenburn's
;; color choices
(cl-flet ((x-color-defined-p))
  (fmakunbound 'x-color-defined-p)
  (require 'compile- nil 'noerror))

(catch 'found
  (dolist (d (mapcar 'file-name-as-directory load-path))
    (let* ((f (expand-file-name (concat d "compile")))
           (regexp (concat (mapconcat 'regexp-quote (get-load-suffixes) "\\|") "$"))
           (seek (car (split-string load-file-name regexp))))
      (and (not (string= f seek))
           (require 'compile f 'noerror)
           (throw 'found nil)))))
