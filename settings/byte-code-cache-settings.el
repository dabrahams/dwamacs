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
