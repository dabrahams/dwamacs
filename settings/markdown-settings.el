(defun markdown-preview-file ()
  "Run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
           (shell-quote-argument (buffer-file-name)))))

(define-key mode-specific-map [?m] 'markdown-preview-file)
