(provide 'code-settings)
(defun my-code-mode-hook ()
  (font-lock-mode t)
  (show-paren-mode t)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(control return)] 'newline)
  (local-set-key [( control ?\( )] 'my-matching-paren)
  
  ;; Try to make completion case sensitive in code buffers.
  (make-local-variable 'dabbrev-case-fold-search)
  (setq dabbrev-case-fold-search nil)
  )

;;;###autoload
(defun my-copyright (&optional copyright)
  "Insert a commented COPYRIGHT string. If COPYRIGHT
is not supplied, the boost copyright is used by default"
  (interactive)
  (let ((copy-start (point)))
      
    (insert-string (concat (or copyright
                       (or (and (my-path-elts) (boost-copyright))
                           (eval (list my-default-copyright))))
                   "\n"))
      
    (comment-region copy-start (point))))
