(require 'filladapt)
(require 'mime-conf)
(require 'wl-summary)
(require 'filladapt)
(require 'wl-conversation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Summary Mode
;;
(define-key wl-summary-mode-map [?D] 'wl-thread-delete)
(define-key wl-summary-mode-map [?d] 'wl-summary-delete)
(define-key wl-summary-mode-map [?b] 'wl-summary-resend-bounced-mail)

(defun dwa/wl-summary-delete-and-move-prev ()
  (interactive)
  (let (wl-summary-move-direction-downward)
    (call-interactively 'wl-summary-delete)))

(define-key wl-summary-mode-map [(control ?d)] 'dwa/wl-summary-delete-and-move-prev)

(add-hook 'wl-summary-mode-hook 'hl-line-mode)

;; Synchronize the folder with the server after executing the summary
;; operation
(add-hook 'wl-summary-exec-hook 'wl-summary-sync-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Address Book
;;

;;; Use ~/.mailrc
(setq wl-address-init-function 'my-wl-address-init)
(defun my-wl-address-init ()
  (wl-local-address-init)
  (setq wl-address-completion-list
        (append wl-address-completion-list (build-mail-aliases))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spam Processing
;;
(require 'wl-spam)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Signatures
;;
(require 'signature)
(setq signature-insert-at-eof t)
(setq signature-delete-blank-lines-at-eof t)

(add-hook
 'wl-init-hook
 '(lambda ()
    ;; Add support for (signature . "filename")
    (unless (assq 'signature wl-draft-config-sub-func-alist)
      (wl-append wl-draft-config-sub-func-alist
                 '((signature . wl-draft-config-sub-signature))))

    (defun mime-edit-insert-signature (&optional arg)
      "Redefine to insert a signature file directly, not as a tag."
      (interactive "P")
      (insert-signature arg))
    ))

(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    ))

(defun wl-draft-config-sub-signature (content)
  "Insert the signature at the end of the MIME message."
  (let ((signature-insert-at-eof nil) ; believe it or not, having this
                                      ; set to t interferes with
                                      ; wl-draft putting the signature
                                      ; at the real end of the buffer.
        (signature-file-name content))
    (goto-char (mime-edit-content-end))
    (insert-signature)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window splitting
;;
(defadvice split-window (before wl-split-horizontally disable)
    "When the system is going to split the summary buffer window
to show a message, I want to split it horizontally!  The default
of splitting vertically (i.e. with a horizontal divider) leaves
me looking at really long lines through really narrow spaces,
which kinda blows."
        (ad-set-arg 2 t)
        (ad-set-arg 1 nil)
        )

(defadvice wl-message-select-buffer (around setup-wl-split-horizontally activate protect)
    "See split-window advice \"wl-split-horizontally\".  Make sure it only applies 
when we need it."
    (ad-enable-advice 'split-window 'before 'wl-split-horizontally)
    (ad-activate 'split-window)
    ad-do-it
    (ad-disable-advice 'split-window 'before 'wl-split-horizontally)
    (ad-activate 'split-window)
    )

(add-hook 'wl-message-redisplay-hook
          (lambda () (let ((growth (- 80 (window-width)))) (> growth 0) (enlarge-window-horizontally growth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message Presentation
;;
(add-hook 'wl-message-buffer-created-hook 'visual-line-mode)

(require 'wl-highlight)
(loop for x in wl-highlight-citation-face-list do 
      (set-face-background x "#F0F0F0"))

;;; Adjustments to be able to display faulty jpg MIME type
;;; per [[http://news.gmane.org/find-root.php?message_id=%3c82bp6fjj71.wl%25kzhr%40d1.dion.ne.jp%3e][Email from Kazuhiro Ito: Re: Counfounding MIME]]
(eval-after-load "mime-image"
  '(let ((rule '(image jpg jpeg)))
     (ctree-set-calist-strictly
      'mime-preview-condition
      (list (cons 'type (car rule))(cons 'subtype (nth 1 rule))
	    '(body . visible)
	    (cons 'body-presentation-method #'mime-display-image)
	    (cons 'image-format (nth 2 rule))))))

;; From [[http://mid.gmane.org/87mxqjn7un.wl%25ucecesf@ucl.ac.uk][Eric S. Fraga]]
;; use visual-line-mode for displaying message.  This is a customization of some
;; code posted by lloyd zusman on the wanderlust mailing list
(defun dwa/summary-redisplay-hook () 
  (save-excursion
    (save-restriction
      (set-buffer wl-message-buffer)
      (save-excursion
	;; (visual-line-mode t) ;; code for reformating the message buffer goes here
	(setq word-wrap t)
	)            
      )))

(add-hook 'wl-summary-redisplay-hook 'dwa/summary-redisplay-hook)

;; to have text flowing automatically in display of emails in wanderlust
(autoload 'fill-flowed "flow-fill")
(defun dwa/flow-fill-mime-display ()
 	    (when (string= "flowed"
 			   (cdr (assoc "format"
 				       (mime-content-type-parameters
 					(mime-entity-content-type entity)))))
 	      (fill-flowed)))
(add-hook 'mime-display-text/plain-hook 'dwa/flow-fill-mime-display)

(defun wl-summary-fill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (with-current-buffer wl-message-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max))))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

(define-key wl-summary-mode-map "\M-q" 'wl-summary-fill-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive Customization
;;

;; Settings
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mime-edit-split-message nil nil nil "
This should really be the default.  Most MUAs can't decode the split messages!")
 '(mime-play-delete-file-immediately nil)
 '(mime-save-directory "/tmp")
 '(mm-attachment-override-types (quote ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/*")) nil nil "
Added image/* to display attached images inline")
 '(mm-discouraged-alternatives (quote ("text/html" "text/richtext" "image/.*")) nil nil "
The documentation for this variable says it all")
 '(mm-inline-text-html-with-images t)
 '(wl-ask-range nil nil nil "
The range thing slows me down.  However, I'd still like to know how to force the question.")
 '(wl-auto-check-folder-list (quote ("Inbox")))
 '(wl-auto-check-folder-name "Inbox")
 '(wl-auto-select-next (quote skip-no-unread))
 '(wl-auto-uncheck-folder-list (quote ("\\$.*" "%zz_mairix.*" "%zz_archive.*" "%.*")))
 '(wl-dispose-folder-alist (quote (("^-" . remove) ("^@" . remove) ("\\<All Mail\\>" . trash) ("^%" . remove))) nil nil "
Only disposing of something in \"All Mail\" should actually move it to
the trash (GMail will clean that up after 30 days).  Otherwise, we can
fully delete it from the current folder because we always have a copy
in \"All Mail.\"
")
 '(wl-draft-always-delete-myself t)
 '(wl-draft-config-alist (quote (("" (part-bottom . "
") ("Bcc" . "Dave Abrahams <dave@boostpro.com>") (signature . "~/.signature")))))
 '(wl-draft-reply-buffer-style (quote full))
 '(wl-draft-sendlog-max-size 100000 nil nil "
Keep more sent messages around for quick/easy access
")
 '(wl-draft-use-cache t nil nil "
This has to be on if I want the 'sendlog folder to contain anything
")
 '(wl-fldmgr-add-complete-with-current-folder-list t)
 '(wl-folder-desktop-name #("Messages" 0 8 (wl-folder-entity-id 0 wl-folder-is-group is-group)))
 '(wl-folder-notify-deleted t)
 '(wl-folder-petname-alist (quote (("%INBOX" . "Inbox") ("+drafts" . "Drafts") (#("%[Gmail]/Sent" 0 13 (wl-folder-is-group nil wl-folder-entity-id 3)) . "Sent") (#("%inbox:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 58 (wl-folder-is-group nil wl-folder-entity-id 72)) . "Inbox") (#("%[Gmail]/Trash:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 66 (wl-folder-is-group nil wl-folder-entity-id 74)) . "Trash") (#("%[Gmail]/Star:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 65 (wl-folder-is-group nil wl-folder-entity-id 75)) . "With a Star") (#("%[Gmail]/Sent:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 65 (wl-folder-is-group nil wl-folder-entity-id 76)) . "Sent") (#("%[Gmail]/Draft:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 66 (wl-folder-is-group nil wl-folder-entity-id 77)) . "Draft") (#("%[Gmail]/All E-Mails:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 72 (wl-folder-is-group nil wl-folder-entity-id 78)) . "All E-Mails") (#("%Org-Mode:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 61 (wl-folder-is-group nil wl-folder-entity-id 79)) . "Org-Mode") (#("%[Gmail]/Draft" 0 14 (wl-folder-is-group nil wl-folder-entity-id 4)) . "Drafts") (#("%[Gmail]/Star" 0 13 (wl-folder-is-group nil wl-folder-entity-id 2)) . "Flagged") ("%Trash" . "Trash") (#("%INBOX" 0 6 (wl-folder-is-group nil wl-folder-entity-id 1)) . "Inbox") (#("%[Gmail]/Starred" 0 16 (wl-folder-is-group nil wl-folder-entity-id 2)) . "Important") (#("%[Gmail]/Sent Mail" 0 18 (wl-folder-is-group nil wl-folder-entity-id 3)) . "Sent") (#("%[Gmail]/Drafts" 0 15 (wl-folder-is-group nil wl-folder-entity-id 4)) . "Drafts") (#("%[Gmail]/All Mail" 0 17 (wl-folder-is-group nil wl-folder-entity-id 5)) . "Archive") (#("%[Gmail]/Trash" 0 14 (wl-folder-is-group nil wl-folder-entity-id 6)) . "Trash") (#("%[Gmail]/Spam" 0 13 (wl-folder-is-group nil wl-folder-entity-id 8)) . "Spam") (#("+draft" 0 6 (wl-folder-is-group nil wl-folder-entity-id 9)) . "Drafts"))))
 '(wl-folder-process-duplicates-alist (quote (("^.*" . hide))) nil nil "
Don't show me any duplicate messages")
 '(wl-folder-window-width 60)
 '(wl-highlight-folder-by-numbers 1)
 '(wl-icon-directory "~/.emacs.d/el-get/wanderlust/icons/")
 '(wl-interactive-exit nil)
 '(wl-interactive-save-folders nil)
 '(wl-interactive-send nil)
 '(wl-message-ignored-field-list (quote ("^.*:")))
 '(wl-message-sort-field-list (quote ("^From" "^Organization:" "^X-Attribution:" "^Subject" "^Date" "^To" "^Cc")))
 '(wl-message-visible-field-list (quote ("^\\(To\\|Cc\\):" "^Subject:" "^\\(From\\|Reply-To\\):" "^Organization:" "^Message-Id:" "^\\(Posted\\|Date\\):" "^\\(Mailer\\|User-Agent\\):" "^\\(List-Post\\):" "^\\(Xref\\):")))
 '(wl-smtp-authenticate-type "plain")
 '(wl-smtp-connection-type (quote starttls))
 '(wl-smtp-posting-port nil)
 '(wl-smtp-posting-server "smtp.gmail.com")
 '(wl-smtp-posting-user "dave@boostpro.com")
 '(wl-spam-folder "%[Gmail]/Spam")
 '(wl-summary-auto-sync-marks nil nil nil "
Trying this setting to see if it improves usability vastly and if I
can tolerate being out-of-sync occasionally.")
 '(wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s")
 '(wl-summary-showto-folder-regexp "^%" nil nil "Show recipient in place of sender in IMAP folders when I'm the sender")
 '(wl-summary-width nil)
 '(wl-thread-insert-opened t)
 '(wl-trash-folder "%Trash" nil nil "
wl-dispose-folder-alist is set up so the only messages sent to Trash
have been marked disposed in an \"All Mail\" folder.  Any others should
be deleted immediately since there is a copy in All Mail.
")
 '(wl-use-folder-petname (quote (modeline ask-folder read-folder)) nil nil "
I have some hard-to-type folder names; why struggle?
")
 '(wl-user-mail-address-list (quote ("dave@boostpro.com" "dave.abrahams@gmail.com" "daveabrahams@gmail.com" "boost.consulting@gmail.com" "david.abrahams@rcn.com" "dave@luannocracy.com"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
