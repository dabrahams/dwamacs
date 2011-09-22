;;; -*- mode: emacs-lisp -*-

;; $Revision: 147 $

;;;_* customizations

;;;_ + variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-after-getting-new-news-hook
   (quote
    (gnus-group-list-groups gnus-display-time-event-handler gnus-score-groups gnus-group-save-newsrc
                            (lambda nil
                              (if
                                  (file-exists-p "/tmp/unread")
                                  (delete-file "/tmp/unread"))
                              (display-time-update)))))
 '(gnus-agent-expire-all t)
 '(gnus-agent-expire-days 14)
 '(gnus-agent-go-online t)
 '(gnus-agent-mark-unread-after-downloaded nil)
 '(gnus-agent-synchronize-flags t)
 '(gnus-always-read-dribble-file t)
 '(gnus-article-address-banner-alist
   (quote
    (("@sig\\.com\\'" . signature)
     ("@volcanocorp\\.com\\'" . signature))))
 '(gnus-article-date-lapsed-new-header t)
 '(gnus-article-sort-functions
   (quote
    ((not gnus-article-sort-by-number))))
 '(gnus-article-update-date-headers nil)
 '(gnus-asynchronous t)
 '(gnus-auto-select-next nil nil nil "
*** VERY IMPORTANT SETTING ***.  See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9399")
 '(gnus-check-new-newsgroups nil)
 '(gnus-cited-closed-text-button-line-format "...<schnipp %n>...
")
 '(gnus-cited-lines-visible
   (quote
    (1 . 4)))
 '(gnus-cited-opened-text-button-line-format "%(%1{[-]%}%)
")
 '(gnus-completing-read-function
   (quote gnus-ido-completing-read))
 '(gnus-default-adaptive-score-alist
   (quote
    ((gnus-dormant-mark
      (from 20)
      (subject 100))
     (gnus-ticked-mark
      (subject 30))
     (gnus-read-mark
      (subject 30))
     (gnus-del-mark
      (subject -150))
     (gnus-catchup-mark
      (subject -150))
     (gnus-killed-mark
      (subject -1000))
     (gnus-expirable-mark
      (from -1000)
      (subject -1000)))))
 '(gnus-default-article-saver
   (quote gnus-summary-write-to-file))
 '(gnus-extra-headers
   (quote
    (To Cc)))
 '(gnus-gcc-mark-as-read t)
 '(gnus-generate-tree-function
   (quote gnus-generate-horizontal-tree))
 '(gnus-group-default-list-level 4)
 '(gnus-group-line-format "%S%p%P%5y%5T: %(%B%G%B%)
")
 '(gnus-group-mode-hook
   (quote
    (gnus-topic-mode gnus-agent-mode)))
 '(gnus-ignored-from-addresses "^david.abrahams@rcn.com\\|dave@boost\\(-consulting\\|pro\\).com$")
 '(gnus-ignored-mime-types
   (quote
    ("application/x-pkcs7-signature" "application/ms-tnef" "text/x-vcard")))
 '(gnus-interactive-exit
   (quote quiet))
 '(gnus-large-newsgroup 1000)
 '(gnus-local-domain "boostpro.com")
 '(gnus-message-archive-group
   (quote
    ((format-time-string "sent.%Y-%m"))))
 '(gnus-message-replyencrypt nil)
 '(gnus-novice-user nil)
 '(gnus-read-active-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method
   (quote
    (current
     (nnir "nnimap:LocalIMAP")
     (nntp "LocalNNTP"
           (nntp-address "localhost")
           (nntp-port-number 9119))
     (nntp "Gmane"
           (nntp-address "news.gmane.org"))
     (nntp "GigaNews"
           (nntp-address "text.giganews.com")
           (nntp-authinfo-user "dabrahams")))))
 '(gnus-refer-thread-use-nnir t)
 '(gnus-registry-ignored-groups
   (quote
    (("nntp" t)
     ("^INBOX" t))))
 '(gnus-safe-html-newsgroups ".")
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-score-default-duration
   (quote p))
 '(gnus-score-expiry-days 30)
 '(gnus-score-find-score-files-function
   (quote
    (gnus-score-find-hierarchical)))
 '(gnus-secondary-select-methods
   (quote
    ((nntp "LocalNNTP"
           (nntp-address "localhost")
           (nntp-port-number 9119)))))
 '(gnus-select-group-hook
   (quote
    (gnus-group-set-timestamp)))
 '(gnus-select-method
   (quote
    (nnimap "LocalIMAP"
            (nnimap-address "localhost")
            (nnimap-user "dave")
            (nnimap-server-port 9143)
            (nnimap-stream network))))
 '(gnus-signature-separator
   (quote
    ("^-- $" "^-- *$" "^_____+$" "^-----+?
NOTICE: ")))
 '(gnus-simplify-subject-functions
   (quote
    (gnus-simplify-subject-fuzzy)))
 '(gnus-sort-gathered-threads-function
   (quote gnus-thread-sort-by-date)
   t)
 '(gnus-spam-process-destinations
   (quote
    (("^\\(\\(nnimap\\+\\)?LocalIMAP:\\)?[^+]*$" "[Gmail].Spam"))))
 '(gnus-spam-process-newsgroups
   (quote
    (("^\\(\\(nntp\\+\\)?LocalNNTP:\\)?gmane\\."
      ((spam spam-use-gmane))))))
 '(gnus-split-methods
   (quote
    ((gnus-save-site-lisp-file)
     (gnus-article-archive-name)
     (gnus-article-nndoc-name))))
 '(gnus-started-hook
   (quote
    ((lambda nil
       (run-hooks
        (quote gnus-after-getting-new-news-hook))))))
 '(gnus-subscribe-newsgroup-method
   (quote gnus-subscribe-topics))
 '(gnus-summary-expunge-below -100)
 '(gnus-summary-line-format "%O%U%R%z%~(form my-align-gnus-summary)@%B%&user-date;: %(%f%~(form my-align-gnus-subject)@%)* %s
")
 '(gnus-summary-mark-below -100)
 '(gnus-suspend-gnus-hook
   (quote
    (gnus-group-save-newsrc)))
 '(gnus-thread-hide-subtree
   (quote
    (not gnus-article-unread-p)))
 '(gnus-thread-sort-functions
   (quote
    ((not gnus-thread-sort-by-number)
     gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-total-score)))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v
")
 '(gnus-treat-date-lapsed
   (quote head))
 '(gnus-treat-from-gravatar
   (quote head))
 '(gnus-treat-hide-citation t)
 '(gnus-treat-mail-gravatar
   (quote head))
 '(gnus-treat-strip-cr t)
 '(gnus-treat-strip-leading-blank-lines t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnus-tree-minimize-window nil)
 '(gnus-uncacheable-groups "^nnml")
 '(gnus-use-adaptive-scoring
   (quote
    (line)))
 '(gnus-use-cache t)
 '(gnus-use-trees t)
 '(gnus-verbose 4)
 '(nnir-hyrex-remove-prefix "~/Library/Data/Gnus/Mail")
 '(nnir-ignored-newsgroups "^\"\\([^[]\\|\\[Gmail]/[^A]\\)")
 '(nnir-imap-default-search-key "imap")
 '(nnir-namazu-index-directory "~/Library/Data/Gnus/Mail/namazu")
 '(nnir-namazu-remove-prefix "~/Library/Data/Gnus/Mail")
 '(nnir-notmuch-remove-prefix "~/Library/Data/Gnus/Mail")
 '(nnir-swish++-configuration-file "~/Library/Data/Gnus/Mail/swish++.conf")
 '(nnir-swish++-remove-prefix "~/Library/Data/Gnus/Mail/")
 '(nnir-swish-e-index-file "~/Library/Data/Gnus/Mail/index.swish-e")
 '(nnir-swish-e-index-files
   (quote
    ("~/Library/Data/Gnus/Mail/index.swish-e")))
 '(nnir-swish-e-remove-prefix "~/Library/Data/Gnus/Mail/")
 '(nnmail-crosspost nil)
 '(nnmail-expiry-wait 30)
 '(nnmail-extra-headers
   (quote
    (To Cc)))
 '(nnmail-scan-directory-mail-source-once t))

;;;_ + faces

;;;_* configuration

(require 'gnus)
(require 'gnus-harvest)
(require 'starttls)
(require 'pgg)

(gnus-harvest-install 'message-x)

(defun my-process-running-p (name)
  (catch 'proc-running
    (dolist (proc (process-list))
      (if (and (string-match name (process-name proc))
               (eq 'run (process-status proc)))
          (throw 'proc-running proc)))))

(defun start-offlineimap ()
  (interactive)
  (shell-command
   "launchctl load -S Aqua -w ~/Library/LaunchAgents/mac.offlineimap.plist")
  (message "Offlineimap started"))

(defun shutdown-offlineimap ()
  (interactive)
  (shell-command
   "launchctl unload -w ~/Library/LaunchAgents/mac.offlineimap.plist")
  (message "Offlineimap stopped"))

(add-hook 'gnus-after-exiting-gnus-hook 'shutdown-offlineimap)

(add-hook 'gnus-summary-mode-hook
          (lambda ()(hl-line-mode 1)))

(autoload 'gnus-dired-mode "gnus-dired" nil t)
(add-hook 'dired-mode-hook 'gnus-dired-mode)

(defun gnus-query (query)
  (interactive "sMail Query: ")
  (let ((nnir-imap-default-search-key "imap"))
    (gnus-group-make-nnir-group
     nil
     `((query    . ,query)
       (criteria . "")
       (server   . "nnimap:LocalIMAP") ))))

(define-key global-map [(alt meta ?f)] 'gnus-query)

(defun gnus-goto-article (message-id)
  (let ((nnir-imap-default-search-key "imap"))
    (gnus-group-make-nnir-group
     nil
     `((query    . ,(concat "header message-id " message-id))
       (criteria . "")
       (server   . "nnimap:LocalIMAP") )))
  (gnus-summary-refer-article message-id))

(defun gnus-current-message-id ()
  (with-current-buffer gnus-original-article-buffer
    (nnheader-narrow-to-headers)
    (message-fetch-field "message-id")))

(defun gnus-open-article-in-apple-mail ()
  (interactive)
  (let ((message-id (gnus-current-message-id)))
    (start-process (concat "open message:" message-id) nil
                   "open" (concat "message://<"
                                  (substring message-id 1 -1) ">"))))

(defun gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "Spam"))

(eval-after-load "gnus-sum"
  '(progn
     (define-key gnus-summary-mode-map [?$] 'gmail-report-spam)
     (define-key gnus-summary-mode-map [?O ?O] 'gnus-open-article-in-apple-mail)
     (define-key gnus-summary-mode-map [?B backspace]
       (function
        (lambda (arg) (interactive "P")
          (if (string-match "\\(drafts\\|queue\\)" gnus-newsgroup-name)
              (gnus-summary-delete-article arg)
            (gnus-summary-move-article arg "[Gmail].Trash")))))
     (define-key gnus-summary-mode-map [(control ?c) (control ?o)]
       'gnus-article-browse-urls)))

(defadvice message-goto-from (after insert-boostpro-address activate)
  (if (looking-back ": ")
      (insert "Dave Abrahams <dave@boostpro.com>"))
  (goto-char (line-end-position))
  (re-search-backward ": ")
  (goto-char (match-end 0)))

(setq my-smtpmailer-alist
      '((".*"
         ("dave@boostpro.com" . "smtp.gmail.com"))
        ))

(defun my-set-smtp-server ()
  (when (message-field-value "to")
    (let* ((to-field (cadr (mail-extract-address-components
                            (message-field-value "to"))))
           (from (let ((field (message-field-value "from")))
                   (and field (cadr (mail-extract-address-components field)))))
           (result
            (car (assoc-default (or from to-field)
                                my-smtpmailer-alist
                                'string-match
                                (cons user-mail-address
                                      (if (boundp 'smtpmail-default-smtp-server)
                                          smtpmail-default-smtp-server
                                        ""))))))
      (if from
          (setq smtpmail-mail-address from
                mail-envelope-from from
                smtpmail-smtp-server (cdr result)
                smtpmail-smtp-service 587)
        ;; set mailer address and port
        (setq smtpmail-mail-address (car result)
              mail-envelope-from (car result)
              smtpmail-smtp-server (cdr result)
              smtpmail-smtp-service 587)
        (message-remove-header "From")
        (message-add-header
         (format "From: %s <%s>" user-full-name (car result)))))))

(add-hook 'message-send-hook 'my-set-smtp-server)

;;;_ + Determine layout of the summary windows

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

;;;_ + Cleanup all Gnus buffers on exit

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
	   (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
	(let (gnus-interactive-exit)
	  (gnus-group-exit)))))

(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;;;_ + Scoring

(eval-when-compile
  (defvar arg))

(defun gnus-score-groups ()
  (interactive)
  (save-excursion
    (dolist (info (cdr gnus-newsrc-alist))
      ;; Only consider this group if it's at or below the current level
      (when (<= (gnus-info-level info)
                (if (numberp arg)
                    arg
                  (or (gnus-group-default-level nil t)
                      (gnus-group-default-list-level)
                      gnus-level-subscribed)))
        (let* ((group (gnus-info-group info))
               (unread (gnus-group-unread group)))
          (when (and (not (string= "nnimap+Local:INBOX" group))
                     (numberp unread) (> unread 0))
            (ignore-errors
              (gnus-summary-read-group group nil t))
            (when (and gnus-summary-buffer
                       (buffer-live-p gnus-summary-buffer)
                       (eq (current-buffer)
                           (get-buffer gnus-summary-buffer)))
              (gnus-summary-exit))))))))

;;;_ + Summary line formats

(defun gnus-user-format-function-Z (header)
  (let ((to (cdr (assq 'To (mail-header-extra header))))
	(newsgroups (cdr (assq 'Newsgroups (mail-header-extra header))))
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (with-current-buffer gnus-summary-buffer
           gnus-newsgroup-ignored-charsets)))
    (cond
     ((and to gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "-> "
	      (or (car (funcall gnus-extract-address-components
				(funcall
				 gnus-decode-encoded-word-function to)))
		  (funcall gnus-decode-encoded-word-function to))))
     ((and newsgroups gnus-ignored-from-addresses
	   (string-match gnus-ignored-from-addresses
			 (mail-header-from header)))
      (concat "=> " newsgroups))
     (t
      (let* ((from (mail-header-from header))
	     (data (condition-case nil
		       (mail-extract-address-components from)
		     (error nil)))
	     (name (car data))
	     (net (car (cdr data))))
	(or name net))))))

(defsubst dot-gnus-tos (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defun gnus-user-format-function-S (header)
  "Return how much time it's been since something was sent."
  (condition-case err
      (let ((date (mail-header-date header)))
	(if (> (length date) 0)
	    (let* ((then (dot-gnus-tos
			  (apply 'encode-time (parse-time-string date))))
		   (now (dot-gnus-tos (current-time)))
		   (diff (- now then)))
	      (cond ((>= diff (* 86400.0 7.0 52.0))
		     (if (>= diff (* 86400.0 7.0 52.0 10.0))
			 (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
		       (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
		    ((>= diff (* 86400.0 30.0))
		     (if (>= diff (* 86400.0 30.0 10.0))
			 (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
		       (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
		    ((>= diff (* 86400.0 7.0))
		     (if (>= diff (* 86400.0 7.0 10.0))
			 (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
		       (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
		    ((>= diff 86400.0)
		     (if (>= diff (* 86400.0 10.0))
			 (format "%3dd" (floor (/ diff 86400.0)))
		       (format "%3.1fd" (/ diff 86400.0))))
		    ((>= diff 3600.0)
		     (if (>= diff (* 3600.0 10.0))
			 (format "%3dh" (floor (/ diff 3600.0)))
		       (format "%3.1fh" (/ diff 3600.0))))
		    ((>= diff 60.0)
		     (if (>= diff (* 60.0 10.0))
			 (format "%3dm" (floor (/ diff 60.0)))
		       (format "%3.1fm" (/ diff 60.0))))
		    (t
		     (format "%3ds" (floor diff)))))))
    (error "    ")))

(eval-when-compile
  (defvar thread)
  (defvar gnus-tmp-level))

(defun gnus-user-format-function-t (header)
  (let ((tcount (gnus-summary-number-of-articles-in-thread
		 (and (boundp 'thread) (car thread)) gnus-tmp-level)))
    (if (> tcount 1)
	(number-to-string tcount)
      " ")))

;;;_ + gnus-article-browse-urls

(defun gnus-article-browse-urls ()
  "Visit a URL from the `gnus-article-buffer' by prompting via a
    poping up a buffer showing the list of URLs found with the
    `gnus-button-url-regexp'."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
        (urls (gnus-article-get-current-urls))
        (this-window (selected-window))
        (browse-window (get-buffer-window gnus-article-buffer))
        (count 0))
    (save-excursion
      (save-window-excursion
        (set-buffer temp-buffer)
        (mapc (lambda (string)
                (insert (format "\t%d: %s\n" count string))
                (setq count (1+ count))) urls)
        (not-modified)
        (pop-to-buffer temp-buffer)
        (setq count
              (string-to-number
               (char-to-string (if (fboundp
                                    'read-char-exclusive)
                                   (read-char-exclusive)
                                 (read-char)))))
        (kill-buffer temp-buffer))
      (if browse-window
          (progn (select-window browse-window)
                 (browse-url (nth count urls)))))
    (select-window this-window)))

(defun gnus-article-get-current-urls ()
  "Return a list of the urls found in the current `gnus-article-buffer'"
  (let (url-list)
    (with-current-buffer gnus-article-buffer
      (setq url-list (gnus-article-get-urls-region (point-min) (point-max))))
    url-list))

(defun gnus-article-get-urls-region (min max)
  "Return a list of urls found in the region between MIN and MAX"
  (let (url-list)
    (save-excursion
      (save-restriction
        (narrow-to-region min max)
        (goto-char (point-min))
        (while (re-search-forward gnus-button-url-regexp nil t)
          (let ((match-string (match-string-no-properties 0)))
            (if (and (not (equal (substring match-string 0 4) "file"))
                     (not (member match-string url-list)))
                (setq url-list (cons match-string url-list)))))))
    url-list))

;;;_* keybindings

;;;_ + gnus-group-score

(eval-after-load "gnus-group"
  '(progn
     (define-key gnus-group-score-map [?s] 'gnus-score-groups)
     (define-key gnus-group-mode-map [?v ?o] 'start-offlineimap)))

(eval-after-load "w3m"
  '(define-key w3m-minor-mode-map "\C-m" 'w3m-view-url-with-external-browser))

;;;_ + dave's stuff

(defun dwa/gnus-summary-ignore-thread ()
  (interactive)
  (gnus-summary-top-thread)
  (let ((message-id (gnus-summary-header "message-id")))
    (dolist (hdr-type '(("references" . s) ("message-id" . e)))
      (gnus-summary-score-entry
       (car hdr-type)                       ; Header
       (gnus-summary-header "message-id")   ; Match
       (cdr hdr-type)                       ; Type
       (- (gnus-score-delta-default nil))   ; Score
       nil                                  ; Temp
       nil                                  ; Prompt
       nil                                  ; not silent
       nil)))                               ; non-standard overview.
  (gnus-summary-hide-thread))
(define-key gnus-summary-mode-map
  [?i] 'dwa/gnus-summary-ignore-thread)

;;
;; Support proportional fonts in the summary and group buffers by inserting a forced alignment
;;
;; See http://news.gmane.org/find-root.php?message_id=%3cyoij63rj41q5.fsf%40remote5.student.chalmers.se%3e
(defvar my-align-gnus-summary (propertize " " 'display '(space :align-to 5)))
(defvar my-align-gnus-subject (propertize " " 'display '(space :align-to 30)))

(defvar my-align-gnus-group (propertize " " 'display '(space :align-to 8)))


;; Display word docs inline with antiword installed.  See
;; http://www.emacswiki.org/emacs/MimeTypesWithGnus
(when nil
  (require 'mm-view)
  (add-to-list 'mm-inline-media-tests
               '("application/msword" mm-inline-text identity))
  (add-to-list 'mm-automatic-external-display "application/msword")
  (add-to-list 'mm-attachment-override-types "application/msword")
  (add-to-list 'mm-automatic-display "application/msword"))

(defun dwa/gnus-summary-first-unread-or-first-subject ()
  "Place the point on the subject line of the first unseen article.
If all article have been seen, on the subject line of the last article."
  (interactive)
  (prog1
      (unless
	  (when (gnus-summary-first-subject t nil t)
	    (gnus-summary-show-thread)
	    (gnus-summary-first-subject t nil t))
	(goto-char (point-min)))))
(setq gnus-auto-select-subject 'dwa/gnus-summary-first-unread-or-first-subject)

;(require 'gnus-spec)
;(eval-when-compile (gnus-compile))

(require 'gravatar nil 'noerror)
(require 'gnus-gravatar nil 'noerror)
(spam-initialize)
(define-key gnus-summary-mode-map
  [?$] 'gnus-summary-mark-as-spam)

(define-key gnus-summary-mode-map
  [?v ?o] 'start-offlineimap)

(require 'gnus)
(require 'gnus-sum)
(require 'mm-util)
(require 'w3m-load)

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

(defvar my-gnus-group-face-attributes '(:family "DejaVu Sans" :weight normal :width condensed))
(defvar my-gnus-summary-face-attributes '(:family "DejaVu Sans" :weight normal :width condensed))

(dolist (facename my-gnus-group-faces)
  (apply 'set-face-attribute facename nil my-gnus-group-face-attributes))
(dolist (facename my-gnus-summary-faces)
  (apply 'set-face-attribute facename nil my-gnus-summary-face-attributes))

;; Make sure cited text has a light gray background, in case people
;; forget to add a blank line after their citations.
(require 'gnus-cite)
(require 'mail-settings)

(loop for x in gnus-cite-face-list do 
      (set-face-attribute x nil ':inherit 'dwa/mail-citation))

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

(xgit-insinuate-gnus)
(dvc-insinuate-gnus)
(provide 'dot-gnus-el)

;;; .gnus.el ends here
