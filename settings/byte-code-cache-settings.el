(defun dwa/delete-corresponding-elc-file ()
  "When the current buffer is an elisp source file, delete
any corresponding compiled .elc file"
  (when (and (buffer-file-name) 
             (string-match "\\`\\(.*\\)[.]el\\(?:[.]gz\\)?\\'" (buffer-file-name)))
    (let ((elc (concat (match-string 1 (buffer-file-name)) ".elc")))
      (when (file-exists-p elc)
        (delete-file elc)))))

;; Be sure not to leave around any outdated .elc files
(add-hook 'before-save-hook 'dwa/delete-corresponding-elc-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bcc-blacklist
   (quote
    ("/\\.recentf$" "/history$" "/\\.newsrc\\.eld$" "/\\.session$" "/gnus-settings\\.el\\'"))))
