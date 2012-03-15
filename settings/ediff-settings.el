(defun keep-mine ()
  (interactive)
  (beginning-of-line)
  (assert (or (looking-at "<<<<<<")
              (re-search-backward "^<<<<<<" nil t)
              (re-search-forward "^<<<<<<" nil t)))
  (goto-char (match-beginning 0))
  (let ((beg (point)))
    (forward-line)
    (delete-region beg (point))
    ;; (re-search-forward "^=======")
    (re-search-forward "^>>>>>>>")
    (setq beg (match-beginning 0))
    ;; (re-search-forward "^>>>>>>>")
    (re-search-forward "^=======")
    (forward-line)
    (delete-region beg (point))))

(defun keep-theirs ()
  (interactive)
  (beginning-of-line)
  (assert (or (looking-at "<<<<<<")
              (re-search-backward "^<<<<<<" nil t)
              (re-search-forward "^<<<<<<" nil t)))
  (goto-char (match-beginning 0))
  (let ((beg (point)))
    ;; (re-search-forward "^=======")
    (re-search-forward "^>>>>>>>")
    (forward-line)
    (delete-region beg (point))
    ;; (re-search-forward "^>>>>>>>")
    (re-search-forward "^#######")
    (beginning-of-line)
    (setq beg (point))
    (re-search-forward "^=======")
    (beginning-of-line)
    (forward-line)
    (delete-region beg (point))))

(defun ediff-keep-both ()
  (interactive)
  (with-current-buffer ediff-buffer-C
    (beginning-of-line)
    (assert (or (looking-at "<<<<<<")
                (re-search-backward "^<<<<<<" nil t)
                (re-search-forward "^<<<<<<" nil t)))
    (beginning-of-line)
    (let ((beg (point)))
      (forward-line)
      (delete-region beg (point))
      (re-search-forward "^>>>>>>>")
      (beginning-of-line)
      (setq beg (point))
      (forward-line)
      (delete-region beg (point))
      (re-search-forward "^#######")
      (beginning-of-line)
      (setq beg (point))
      (re-search-forward "^=======")
      (beginning-of-line)
      (forward-line)
      (delete-region beg (point)))))

(add-hook 'ediff-keymap-setup-hook 
          (lambda ()
            (define-key ediff-mode-map [?c] 'ediff-keep-both)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-custom-diff-options "-u" nil nil "
Show me unified diffs by default")
 '(ediff-highlight-all-diffs nil nil nil "
only highlight the selected diff (keeps down gray cruft onscreen)")
 '(ediff-keep-variants t nil nil "
Any unchanged buffers in the ediff are removed when the session ends. 
`C-u q' to override when quitting.")
 '(ediff-merge-filename-prefix "")
 '(ediff-skip-merge-regions-that-differ-from-default nil)
 '(ediff-split-window-function
   (quote split-window-horizontally)
   nil nil "
Show diffs side-by-side")
 '(ediff-window-setup-function
   (quote ediff-setup-windows-plain)
   nil nil "
Run Ediff all in one frame.  The default when there's a window manager is for
emacs to pop up a separate frame for the `*Ediff Control Panel*' buffer"))
