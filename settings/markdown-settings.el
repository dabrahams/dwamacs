(defun markdown-preview-file ()
  "Run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
           (shell-quote-argument (buffer-file-name)))))

(define-key mode-specific-map [?m] 'markdown-preview-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "markdown-extra"))
