
;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-article-update-date-headers 300 nil nil "Update article date every 5 minutes.  Without this it's like having a timer that updates every second.")
 '(gnus-auto-select-next
   (quote slightly-quietly)
   nil nil "**VERY IMPORTANT SETTING**
Without it I have a tendency to hold down the wrong key by mistake
and unintentionally mark many articles read and then leave the group
without a chance to even see what they were.")
 '(gnus-buttonized-mime-types
   (quote
    ("multipart/signed" "multipart/alternative" "application/msword")))
 '(gnus-check-new-newsgroups nil nil nil "Save time at startup by not checking for new newsgroups")
 '(gnus-extra-headers
   (quote
    (To Newsgroups X-Spambayes-Classification Reply-To Message-ID Message-Id)))
 '(gnus-gravatar-style
   (quote inline))
 '(gnus-group-line-format "%E%M%S%p%P%5y%~(form my-align-gnus-group)@|%B%(%G%)
")
 '(gnus-ignored-from-addresses "^david.abrahams@rcn.com\\|dave@boost\\(-consulting\\|pro\\).com$")
 '(gnus-large-newsgroup 5000)
 '(gnus-novice-user nil)
 '(gnus-picon-style
   (quote right))
 '(gnus-read-active-file t)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method
   (quote
    (current
     (nnir)
     (nnregistry)
     (nntp "news.gmane.org")
     (nnir "nnimap:BoostPro"))))
 '(gnus-registry-ignored-groups
   (quote
    (("delayed$" t)
     ("drafts$" t)
     ("queue$" t)
     ("INBOX$" t)
     ("^nnmairix:" t)
     ("archive" t)
     ("^nntp:" t))))
 '(gnus-registry-install t)
 '(gnus-registry-max-entries 10000)
 '(gnus-save-newsrc-file nil)
 '(gnus-secondary-select-methods
   (quote
    ((nntp "LocalNNTP"
           (nntp-address "localhost")
           (nntp-port-number 9119)))))
 '(gnus-select-method
   (quote
    (nnimap "LocalIMAP"
            (nnimap-address "localhost")
            (nnimap-user "dave")
            (nnimap-server-port 9143)
            (nnimap-stream network))))
 '(gnus-spam-process-destinations
   (quote
    (("INBOX" "[Gmail]/Spam"))))
 '(gnus-spam-process-newsgroups
   (quote
    (("INBOX" nil))))
 '(gnus-summary-ignore-duplicates t)
 '(gnus-treat-fill-long-lines
   (quote first)
   nil nil "
Some people don't embed linebreaks in their paragraphs; this will force-add them.")
 '(gnus-treat-from-gravatar
   (quote head))
 '(gnus-treat-from-picon
   (quote head))
 '(gnus-treat-mail-gravatar
   (quote head))
 '(gnus-treat-mail-picon
   (quote head))
 '(gnus-treat-newsgroups-picon
   (quote head))
 '(gnus-treat-strip-cr t)
 '(nnir-ignored-newsgroups "^\"\\([^[]\\|\\[Gmail][./][^A]\\)" nil nil "Only search in Gmail's \"All Mail\" group.  
Emacs regexps don't support negative matches, so this is about the best we can do.")
 '(nnmail-extra-headers
   (quote
    (To Newsgroups Cc))))
