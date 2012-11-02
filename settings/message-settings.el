(defun message-insert-spam-resistant-citation-line ()
  "Insert a simple citation line that slightly obscures the email address of the sender."

  (when message-reply-headers
    (insert "\non " 
            (format-time-string 
             "%a %b %d %Y" (date-to-time (mail-header-date message-reply-headers))) 
            ", "
            (replace-regexp-in-string "@" "-AT-" (mail-header-from message-reply-headers))
            " wrote:\n\n")) )

(defun org-message-store-link ()
  (when (eq major-mode 'message-mode)
    (save-restriction
      (nnheader-narrow-to-headers)
      (org-store-link-props
       :type "message"
       :link (org-make-link "message://" (substring (message-fetch-field "message-id" t) 1 -1))
       :description (message-fetch-field "subject")))
    t))

(defun dwa/index-all-mail ()
  (start-process "doveadm-index" nil "/usr/local/bin/doveadm" "-vvv" "index" "[Gmail].All Mail"))
  
(defun org-message-buffer-store-link ()
  (require 'org)
  (let ((org-store-link-functions '(org-message-store-link)))
    (call-interactively 'org-store-link)))

(add-hook 'org-store-link-functions 'org-message-store-link)
(add-hook 'org-store-link-functions 'dwa/index-all-mail)
(add-hook 'message-sent-hook 'org-message-buffer-store-link)

(defun dwa/message-send-rename ()
  "Renames sent message buffers to include the Subject field when
used as the value of `message-send-rename-function'."
  (message-narrow-to-headers)
  (unwind-protect
      (when (string-match
             "\\`\\*\\(sent \\|unsent \\)?\\(.+\\)\\*[^\\*]*\\|\\`mail to "
             (buffer-name))
        (let ((name (match-string 2 (buffer-name)))
              to group)
          (if (not (or (null name)
                       (string-equal name "mail")
                       (string-equal name "posting")))
              (setq name (concat "*sent " name ": "))
            (message-narrow-to-headers)
            (setq to (message-fetch-field "to"))
            (setq group (message-fetch-field "newsgroups"))
            (widen)
            (setq name
                  (cond
                   (to (concat "*sent mail to "
                               (or (car (mail-extract-address-components to))
                                   to) ": "))
                   ((and group (not (string= group "")))
                    (concat "*sent posting on " group ": "))
                   (t "*sent mail: "))))
          (setq name (concat name (message-fetch-field "subject") "*"))
          (unless (string-equal name (buffer-name))
            (rename-buffer name t))))
    (widen)))

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
    ("^Content-Transfer-Encoding:" "^X-Gnus" "^X-" "^Received:" "^User-Agent:" "^Face:" "^References:")))
 '(message-forward-show-mml t)
 '(message-log-max 1000)
 '(message-mode-hook
   (quote
    ((lambda nil
       (auto-fill-mode t))))
   nil nil "
Automatically wrap text during email composition")
 '(message-send-mail-function
   (quote message-send-mail-with-sendmail))
 '(message-send-rename-function
   (quote dwa/message-send-rename))
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
