(add-to-list 'exec-path "C:/msysgit/bin")
(add-to-list 'exec-path "C:/msysgit/cmd")

(setenv "PATH" (mapconcat (lambda (s) (replace-regexp-in-string "/" "\\\\" s)) exec-path ";"))

(let ((existing-font (assoc 'font default-frame-alist)))
  (if existing-font
    (setcdr existing-font "Lucida Console-12")
    (add-to-list 'default-frame-alist '(font . "Lucida Console-12"))))
  

(deftheme system-type-windows-nt
  "Created 2012-08-27.")

(provide-theme 'system-type-windows-nt)
