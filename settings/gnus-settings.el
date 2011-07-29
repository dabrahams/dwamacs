;;
;; Support proportional fonts in the summary and group buffers by inserting a forced alignment
;;
;; See http://news.gmane.org/find-root.php?message_id=%3cyoij63rj41q5.fsf%40remote5.student.chalmers.se%3e
(defvar my-align-gnus-summary (propertize " " 'display '(space :align-to 5)))
(defvar my-align-gnus-subject (propertize " " 'display '(space :align-to 30)))

;; The default gnus-summary-line-format is:
;;
;; "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n
;;
;; %U   Status of this article (character, "R", "K", "-" or " ")
;; %R   "A" if this article has been replied to, " " otherwise (character)
;; %z   Article zcore (character)
;; %I   Indentation based on thread level (a string of spaces)
;; %(   *** Highlighted when the mouse hovers ***
;;   %[   Opening bracket (character, "[" or "<")
;;   %]   Closing bracket (character, "]" or ">")
;;   %L   Number of lines in the article (integer)  *** in a field of 4 characters ***
;;   :    Literal
;;   %f   Contents of the From: or To: headers (string)  *** -23,23
;; %)   *** Highlighted when the mouse hovers ***

(setq gnus-summary-line-format "%O%U%R%z%~(form my-align-gnus-summary)@%B%&user-date;: %(%f%~(form my-align-gnus-subject)@%)* %s\n")

(defvar my-align-gnus-group (propertize " " 'display '(space :align-to 8)))

(defun my-gnus-started-hook()
  (set-frame-name "Gnus")
  (ignore-errors
   (require 'w3m-load))

  ;; Display word docs inline with antiword installed.  See
  ;; http://www.emacswiki.org/emacs/MimeTypesWithGnus
    (add-to-list 'mm-inline-media-tests
                 '("application/msword" mm-inline-text identity))
    (add-to-list 'mm-automatic-external-display "application/msword")
    (add-to-list 'mm-attachment-override-types "application/msword")
    (add-to-list 'mm-automatic-display "application/msword")
)

(setq gnus-started-hook 'my-gnus-started-hook)

(require 'gnus)
(require 'gnus-sum)
(require 'mm-util)

(ignore-errors 
  (require 'gnus-gravatar))

(spam-initialize)
(gnus-registry-initialize)

(define-key gnus-summary-mode-map
  "$" 'gnus-summary-mark-as-spam)

(defvar my-gnus-group-faces
  '(
    gnus-group-news-1
    gnus-group-news-1-empty
    gnus-group-news-2
    gnus-group-news-2-empty
    gnus-group-news-3
    gnus-group-news-3-empty
    gnus-group-news-4
    gnus-group-news-4-empty
    gnus-group-news-5
    gnus-group-news-5-empty
    gnus-group-news-6
    gnus-group-news-6-empty
    gnus-group-news-low
    gnus-group-news-low-empty
    gnus-group-mail-1
    gnus-group-mail-1-empty
    gnus-group-mail-2
    gnus-group-mail-2-empty
    gnus-group-mail-3
    gnus-group-mail-3-empty
    gnus-group-mail-low
    gnus-group-mail-low-empty))

(defvar my-gnus-summary-faces 
  '(
    gnus-summary-selected
    gnus-summary-cancelled
    gnus-summary-high-ticked
    gnus-summary-low-ticked
    gnus-summary-normal-ticked
    gnus-summary-high-ancient
    gnus-summary-low-ancient
    gnus-summary-normal-ancient
    gnus-summary-high-undownloaded
    gnus-summary-low-undownloaded
    gnus-summary-normal-undownloaded
    gnus-summary-high-unread
    gnus-summary-low-unread
    gnus-summary-normal-unread
    gnus-summary-high-read
    gnus-summary-low-read
    gnus-summary-normal-read))

(defvar my-gnus-group-face-attributes '(:family "Helvetica" :weight normal :width condensed))
(defvar my-gnus-summary-face-attributes '(:family "Helvetica" :weight normal :width condensed))

(dolist (facename my-gnus-group-faces)
  (apply 'set-face-attribute facename nil my-gnus-group-face-attributes))
(dolist (facename my-gnus-summary-faces)
  (apply 'set-face-attribute facename nil my-gnus-summary-face-attributes))

(if t
    (progn
      (gnus-add-configuration
       '(article
         (horizontal 1.0
                     (vertical 1.0
                               (group 35)
                               (summary 1.0 point))
                     (vertical .5 (article 1.0)))))


      (gnus-add-configuration
       '(summary
         (horizontal 1.0
                     (vertical 1.0
                               (group 35)
                               (summary 1.0 point))
                     (vertical .5 (article 1.0)))))

      (gnus-add-configuration
       '(message
         (horizontal 1.0
                     (vertical 1.0
                               (group 35)
                               (summary 1.0))
                     (vertical .5
                               (message 1.0 point)))))

      (gnus-add-configuration
       '(reply
         (horizontal 1.0
                     (vertical 1.0
                               (group 35)
                               (summary 1.0))
                     (vertical .5
                               (message 1.0 point)
                               (article .25)))))

      (gnus-add-configuration
       '(reply-yank
         (horizontal 1.0
                     (vertical 1.0
                               (group 35)
                               (summary 1.0))
                     (vertical .5
                               (message 1.0 point)))))
      )
  
  (progn
      (gnus-add-configuration
       '(article
         (horizontal 1.0 
                     (group 60)
                     (summary 1.0 point)
                     (article 80))))


      (gnus-add-configuration
       '(summary
         (horizontal 1.0 
                     (group 60)
                     (summary 1.0 point)
                     (article 80))))

      (gnus-add-configuration
       '(message
         (horizontal 1.0 
                     (group 60)
                     (summary 1.0)
                     (message 80 point))))

      (gnus-add-configuration
       '(reply
         (horizontal 1.0 
                     (group 60)
                     (summary 1.0)
                     (message 80 point))))

      (gnus-add-configuration
       '(reply-yank
         (horizontal 1.0 
                     (group 60)
                     (summary 1.0)
                     (message 80 point))))
    ))

;; Make sure cited text has a light gray background, in case people
;; forget to add a blank line after their citations.
(require 'gnus-cite)
(require 'mail-settings)

(loop for x in gnus-cite-face-list do 
      (set-face-attribute x nil ':inherit 'dwa/mail-citation))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(require 'gnus-spec)
(require 'gravatar)

(gnus-compile)

;; Thanks to David Engster
;; [[gnus:nntp%2Bnews.gmane.org:gmane.emacs.gnus.general#87vdnimyxd.fsf@randomsample.de][Posting on ding@gnus.org]]
(defun DE-collapse-group-names ()
  (save-excursion
    (let (previous-group current-group common-prefix
			 common-dot-count prefix suffix)
      (goto-char (point-min))
      (while (not (eobp))
	(when (setq current-group 
		    (get-text-property (point) 'gnus-group))
	  (setq current-group (symbol-name current-group))
	  (when (string-match "\\(.+\\):\\(.+\\)" current-group)
	    (setq current-group (match-string 2 current-group)))
	  (setq common-prefix (substring current-group 0 
					 (mismatch previous-group current-group))
		common-dot-count (count ?. common-prefix)
		prefix (mapconcat (lambda (x) x) 
				  (make-list common-dot-count "  .") "")
		suffix (and (string-match
			     (format "\\([^.]*[.]\\)\\{%d\\}\\(.+\\)" common-dot-count) 
			     current-group)
			    (match-string 2 current-group))
		previous-group current-group)
	  (unless (zerop (length prefix))
	    (when (search-forward current-group (point-at-eol) t)
	      (let ((props (text-properties-at (1- (point)))))
		(replace-match (apply 'propertize (concat prefix suffix)
				      props))))))
	(forward-line 1)))))

(add-hook 'gnus-group-prepare-hook 'DE-collapse-group-names)
(add-hook 'gnus-group-update-group-hook 'DE-collapse-group-names)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-article-update-date-headers 300 nil nil "Update article date every 5 minutes.  Without this it's like having a timer that updates every second.")
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
     (nntp "news.gmane.org"))))
 '(gnus-registry-install t)
 '(gnus-save-newsrc-file nil)
 '(gnus-secondary-select-methods
   (quote
    ((nntp "news.gmane.org"))))
 '(gnus-select-method
   (quote
    (nnimap "BoostPro"
            (nnimap-address "imap.gmail.com")
            (nnimap-stream ssl)
            (nnimap-authenticator login))))
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
 '(nnir-ignored-newsgroups "^\"\\([^[]\\|\\[Gmail]/[^A]\\)" nil nil "Only search in Gmail's \"All Mail\" group.  
Emacs regexps don't support negative matches, so this is about the best we can do."))
