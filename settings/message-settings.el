(defun message-insert-spam-resistant-citation-line ()
  "Insert a simple citation line that slightly obscures the email address of the sender."

  (when message-reply-headers
    (insert "\non " 
            (format-time-string 
             "%a %b %d %Y" (date-to-time (mail-header-date message-reply-headers))) 
            ", "
            (replace-regexp-in-string "@" "-AT-" (mail-header-from message-reply-headers))
            " wrote:\n\n")) )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(message-citation-line-function
   (quote message-insert-spam-resistant-citation-line))
 '(message-cite-prefix-regexp "\\([ 	]*[_.[:word:]]+>+\\|[ 	]*[]>|]\\)+" nil nil "
Removed \"}\" from the allowable characters because I often type that when writing replies.")
 '(message-default-headers "Bcc: dave@boostpro.com
" nil nil "
Always Bcc: myself")
 '(message-forward-ignored-headers
   (quote
    ("^Content-Transfer-Encoding:" "^X-Gnus" "^X-" "^Received:" "^User-Agent:" "^References:")))
 '(message-mode-hook
   (quote
    ((lambda nil
       (auto-fill-mode t))))
   nil nil "
Automatically wrap text during email composition")
 '(message-send-mail-function
   (quote message-send-mail-with-sendmail))
 '(message-subject-re-regexp "^[ 	]*\\(\\([Rr][Ee]\\|[Aa][Ww]\\)\\(\\[[0-9]*\\]\\)*:[ 	]*\\)*[ 	]*" nil nil "
Handle Germans' Aw: version of Re:")
 '(message-subject-trailing-was-query t nil nil "
always strip the trailing old subject in (was: ...) subjects")
 '(message-syntax-checks
   (quote
    ((sender . disabled)
     (long-lines . disabled)))
   nil nil "
Don't complain about long lines, please"))
