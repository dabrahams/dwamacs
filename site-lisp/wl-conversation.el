;; Much credit for this code goes to Eric G. Hetzner
(require 'org-wl)

(defun dwa:wl-all-folder () "%[Gmail].All Mail")

(defvar dwa:wl-summary-prev-folder-name nil)
(defvar dwa:wl-summary-prev-message-id nil)


(defun dwa:wl-buffer-thread-location ()
  "Return a pair consisting of the message-id of the message in
the current buffer and of the root of its thread (both surrounded
by <...>)"
    (let ((message-id (std11-field-body "Message-Id")))
     ;; The thread root is the first UID in References, if any, or
     ;; else is the current message
      (cons message-id
            (car (split-string (or (std11-field-body "References") message-id)))))
    )

(defun dwa:wl-current-thread-location ()
  "Return a pair consisting of the message-id of the current
message and of the root of its thread (both surrounded by <...>)"
  (save-window-excursion
    (if (string= mode-name "MIME-View")
        (mime-preview-quit))
    (wl-summary-set-message-buffer-or-redisplay)
    (set-buffer (wl-message-get-original-buffer))
    (dwa:wl-buffer-thread-location)))

(defun dwa:wl-thread-root-folder (thread-root)
  (let ((root-uid (substring thread-root 1 -1)))
         (concat "/message-id:\"" thread-root
                 "\"|references:\"" thread-root
                 "\"/" (dwa:wl-all-folder))))

(defun dwa:wl-summary-visit-conversation (&optional close)
 (interactive "P")
 (if close
     (if dwa:wl-summary-prev-folder-name
         (progn
           (wl-summary-goto-folder-subr dwa:wl-summary-prev-folder-name
                                        'no-sync nil nil t)
           (wl-summary-jump-to-msg-by-message-id dwa:wl-summary-prev-message-id))
       (message "No previous folder to visit."))

   (let* ((thread-location (dwa:wl-current-thread-location))
          (cur-message-id (car thread-location))
          (thread-folder (dwa:wl-thread-root-folder (cdr thread-location)))
          (prev-folder-name wl-summary-buffer-folder-name)
         )
     (wl-summary-goto-folder-subr thread-folder 'update nil nil t)
     (wl-summary-jump-to-msg-by-message-id cur-message-id)
     (setq dwa:wl-summary-prev-folder-name prev-folder-name
           dwa:wl-summary-prev-message-id cur-message-id)
     (make-local-variable 'dwa:wl-summary-prev-folder-name)
     (make-local-variable 'dwa:wl-summary-prev-message-id)
     )))

(defun dwa:org-message-buffer-store-link-impl ()
  "Store an org link to the message in the current buffer, in the
context of its thread"
  (let* ((thread-location (dwa:wl-buffer-thread-location))
         (message-id (car thread-location))
         (folder-name (dwa:wl-thread-root-folder (cdr thread-location)))
         (message-id-no-brackets (org-remove-angle-brackets message-id))
         (link (concat "wl:" folder-name "#" message-id-no-brackets))
             )
        (org-store-link-props :type "wl" :from (std11-field-body "From") :to (std11-field-body "To")
                              :subject (std11-field-body "Subject") :message-id message-id
                              :link link)
        (org-add-link-props :description (org-email-link-description))
    )
  t)

(defun dwa:org-message-buffer-store-link ()
  (require 'org)
  (let ((org-store-link-functions '(dwa:org-message-buffer-store-link-impl)))
    (call-interactively 'org-store-link)))

(add-to-list 'wl-mail-send-pre-hook 'dwa:org-message-buffer-store-link)

(defadvice org-wl-store-link-message (after dwa:org-wl-store-link activate protect)
  (if (string= (substring ad-return-value 0 3) "wl:")
      (let* ((thread-location (dwa:wl-current-thread-location))
             (message-id (car thread-location))
             (folder-name (dwa:wl-thread-root-folder (cdr thread-location)))
             (message-id-no-brackets (org-remove-angle-brackets message-id))
             (link (concat "wl:" folder-name "#" message-id-no-brackets))
             )
        (org-add-link-props :link link)
        (setq ad-return-value link)
    )))

     ;; Use `bogofilter' as spam back end
     ;; Set `scheme' here as the spam filter you will use.
     ;; *Note Spam Filter Processors::.
     (setq elmo-spam-scheme 'bogofilter)
     (require 'wl-spam)

(define-key wl-summary-mode-map "X" 'dwa:wl-summary-visit-conversation)

(provide 'wl-conversation)
