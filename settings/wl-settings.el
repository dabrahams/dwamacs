(require 'filladapt)
(require 'mime-conf)
(require 'wl-summary)
(require 'filladapt)
(require 'wl-conversation)
(ignore-errors
  (require 'wl-gravatar))

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
;Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    ;; (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    ))

;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))

(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; ---------- Some nice stuff from Mat ----------
(add-hook 'wl-summary-prepared-hook
         (lambda ()
           (setq wl-summary-buffer-exit-function
                 (when (eq 'filter
                           (elmo-folder-type-internal
wl-summary-buffer-elmo-folder))
                   'wl-summary-unvirtual))))

(defun wl-summary-filter-unread-important ()
 "Make a virtual folder containing only unread or important messages."
 (interactive)
 (if (eq 'filter
           (elmo-folder-type-internal wl-summary-buffer-elmo-folder))
     (wl-summary-unvirtual)
   (wl-summary-goto-folder-subr
                (concat "/flag:important|flag:unread/"
(wl-summary-buffer-folder-name))
                'update nil nil t)))

(define-key wl-summary-mode-map "\C-co" 'wl-summary-filter-unread-important)


;; ----------------------------------------------------------------------------
;;; User Functions

(defun my-wl-draft-kill-force ()
  (interactive)
  (wl-draft-kill t))

(defun my-wl-delete-whole-folder ()
  (interactive)
  (wl-summary-target-mark-all)
  (wl-summary-target-mark-delete)
  (wl-summary-exec)
  (wl-summary-exit))

(defun my-wl-check-mail-primary ()
  (interactive)
  (unless (get-buffer wl-folder-buffer-name)
    (wl))
  (delete-other-windows)
  (switch-to-buffer wl-folder-buffer-name)
  (goto-char (point-min))
  (next-line 1)
  (wl-folder-jump-to-current-entity))

(defun my-wl-auto-save-draft-buffers ()
  (let ((buffers (wl-collect-draft)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (if (buffer-modified-p) (wl-draft-save))
        (setq buffers (cdr buffers))))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(defun my-wl-summary-delete-and-move-prev ()
  (interactive)
  (let (wl-summary-move-direction-downward)
    (call-interactively 'wl-summary-delete)))

(defun my-wl-summary-goto-to-folder (folder)
  "Goto FOLDER from the summary buffer after closing it."
  (wl-summary-exit t)
  (set-buffer (get-buffer wl-folder-buffer-name))
  (wl-folder-goto-folder-subr folder))

(defun my-wl-goto-to-folder (folder)
  "Goto FOLDER from either the folders or summary buffer after closing it."
  (if (string= (buffer-name) wl-summary-buffer-name)
      (my-wl-summary-goto-to-folder search-folder)
    (wl-folder-goto-folder-subr search-folder)))

(defun my-clean-mime-reply ()
  "Clean-up the citation in replies, removing unnecessary entities."
  (interactive)
  (require 'misc-cmds)
  ;; Find and strip the first tag, indicating the start of the
  ;; cited message
  (when (re-search-forward "^> \\[1" nil t)
    (beginning-of-line)
    (delete-lines 1)
    (while (or (looking-at "^> *$")
               (looking-at "^> \\[[1-9]"))
      (delete-lines 1))
    (when (re-search-forward "^> \\[[1-9][\\. ]" nil t)
      (beginning-of-line)
      (let ((pt (point)))
        (re-search-forward "^$")
        (delete-region pt (point)))))
  ;; Now find the tag that ends the first section, and strip off
  ;; everything from there to the end of the message (including any
  ;; other sections that got cited)
  (goto-char (point-max))
  (when (re-search-backward "^> +[^ ]" nil t)
    (beginning-of-line)
    (let ((pt (point)))
      (goto-char (point-max))
      (if (re-search-backward "^> *$" pt t)
          (progn
            (beginning-of-line)
            (while (looking-at "^> *$")
              (delete-lines 1)
              (forward-line -1))
            (forward-line 1)
            (delete-lines 1))
        (goto-char (point-max))
        (re-search-backward "^$")
        (delete-lines 1)))))

(defun wl-rehilight ()
  "Re-highlight message."
  (let ((beg (point-min))
        (end (point-max)))
    (put-text-property beg end 'face nil)
    (wl-highlight-message beg end t)))

;; (defadvice wl-summary-reply-with-citation 
;;   (around wl-summary-reply-sans-mime-buttons activate )
;;     (let (mime-view-buttons-visible)
;;       (wl-message-buffer-cache-clean-up)
;;       (wl-summary-redisplay)
;;       ad-do-it))

(defun dwa/wl-highlight-hook (beg end len)
  (let ((begining (or (save-excursion
			(goto-char beg)
			(re-search-backward "^" nil t))
		      (point-min)))
	(ending (or (save-excursion
		      (goto-char end)
		      (re-search-forward "$" nil t))
		    (point-max))))
    (put-text-property begining ending 'face nil)
    (wl-highlight-message begining ending t nil)
    (wl-highlight-message begining ending t t)))

(add-hook 'wl-draft-mode-hook
          (lambda ()
            (make-local-variable
             'after-change-functions)
            (add-hook 'after-change-functions
                      'dwa/wl-highlight-hook)))

(defun my-mime-save-content-find-file (entity &optional situation)
  "Save the attached mime ENTITY and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (let* ((name (or (mime-entity-safe-filename entity)
                   (format "%s" (mime-entity-media-type entity))))
         (dir (if (eq t mime-save-directory)
                  default-directory
                mime-save-directory))
         (filename (expand-file-name
                    (file-name-nondirectory name) temporary-file-directory)))
    (mime-write-entity-content entity filename)
    (select-frame (make-frame))
    (find-file filename)
    ))

(defun my-mime-view-emacs-mode (entity &optional situation)
  "Internal method for mime-view to display the mime ENTITY in a buffer with an
appropriate emacs mode."
  (let ((buf (get-buffer-create
              (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      ;;(mule-caesar-region (point-min) (point-max))
      ;; Set emacs mode here
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
          (select-window (or win (get-largest-window)))
          ))
    (view-buffer buf)
    (goto-char (point-min))
    ))

(defun my-mime-find-file-current-entity ()
  "Save the current mime entity and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
        (my-mime-save-content-find-file entity)))
  )

(defun my-mime-button-exec ()
  "Execute the button under point without using the mouse."
  (interactive)
  (let (buf point func data)
    (save-window-excursion
      (setq buf (current-buffer)
            point (point)
            func (get-text-property (point) 'mime-button-callback)
            data (get-text-property (point) 'mime-button-data)
            ))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
          (apply func data))
      )))

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
          (lambda () (let ((growth (- 80 (window-width)))) (if (> growth 0) (enlarge-window-horizontally growth)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message Presentation
;;
(add-hook 'wl-message-buffer-created-hook 'visual-line-mode)

(require 'wl-highlight)
(loop for x in wl-highlight-citation-face-list do 
      (set-face-background x "#F0F0F0"))

(defadvice color-theme-zenburn (after wl-zenburn-setup activate)
  (loop for x in wl-highlight-citation-face-list do 
        (set-face-background x zenburn-bg+1)))

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
 '(elmo-imap4-default-authenticate-type
   (quote clear))
 '(elmo-imap4-default-port 993)
 '(elmo-imap4-default-server "imap.gmail.com")
 '(elmo-imap4-default-stream-type
   (quote ssl))
 '(elmo-imap4-default-user "dave@boostpro.com")
 '(elmo-lang "en")
 '(elmo-localdir-folder-path "~/Maildir")
 '(elmo-message-fetch-confirm nil)
 '(elmo-message-fetch-threshold 250000 nil nil "
The default limit is so low that it always asks about messages that would fetch quickly.")
 '(elmo-network-session-idle-timeout 300 nil nil "James Harkins' alternative to the nasty workaround in commit b1692de9 of dwamacs")
 '(elmo-nntp-default-server "news.gmane.org")
 '(elmo-search-namazu-default-index-path "~/Maildir")
 '(elmo-spam-scheme
   (quote bogofilter))
 '(mime-edit-split-message nil nil nil "
This should really be the default.  Most MUAs can't decode the split messages!")
 '(mime-play-delete-file-immediately nil)
 '(mime-save-directory "/tmp")
 '(mm-attachment-override-types
   (quote
    ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/*"))
   nil nil "
Added image/* to display attached images inline")
 '(mm-discouraged-alternatives
   (quote
    ("text/html" "text/richtext" "image/.*"))
   nil nil "
The documentation for this variable says it all")
 '(mm-inline-text-html-with-images t)
 '(wl-ask-range nil nil nil "
The range thing slows me down.  However, I'd still like to know how to force the question.")
 '(wl-auto-check-folder-list
   (quote
    ("Inbox")))
 '(wl-auto-check-folder-name "Inbox")
 '(wl-auto-select-next
   (quote skip-no-unread))
 '(wl-auto-uncheck-folder-list
   (quote
    ("\\$.*" "%zz_mairix.*" "%zz_archive.*" "%.*")))
 '(wl-dispose-folder-alist
   (quote
    (("^-" . remove)
     ("^@" . remove)
     ("\\<All Mail\\>" . trash)
     ("^%" . remove)))
   nil nil "
Only disposing of something in \"All Mail\" should actually move it to
the trash (GMail will clean that up after 30 days).  Otherwise, we can
fully delete it from the current folder because we always have a copy
in \"All Mail.\"
")
 '(wl-draft-always-delete-myself t)
 '(wl-draft-config-alist
   (quote
    ((""
      (part-bottom . "
")
      ("Bcc" . "Dave Abrahams <dave@boostpro.com>")
      (signature . "~/.signature")))))
 '(wl-draft-reply-buffer-style
   (quote full))
 '(wl-draft-sendlog-max-size 100000 nil nil "
Keep more sent messages around for quick/easy access
")
 '(wl-draft-use-cache t nil nil "
This has to be on if I want the 'sendlog folder to contain anything
")
 '(wl-fldmgr-add-complete-with-current-folder-list t)
 '(wl-folder-desktop-name
   #("Messages" 0 8
     (wl-folder-entity-id 0 wl-folder-is-group is-group)))
 '(wl-folder-notify-deleted t)
 '(wl-folder-petname-alist
   (quote
    (("%INBOX" . "Inbox")
     ("+drafts" . "Drafts")
     (#("%[Gmail]/Sent" 0 13
        (wl-folder-is-group nil wl-folder-entity-id 3))
      . "Sent")
     (#("%inbox:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 58
        (wl-folder-is-group nil wl-folder-entity-id 72))
      . "Inbox")
     (#("%[Gmail]/Trash:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 66
        (wl-folder-is-group nil wl-folder-entity-id 74))
      . "Trash")
     (#("%[Gmail]/Star:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 65
        (wl-folder-is-group nil wl-folder-entity-id 75))
      . "With a Star")
     (#("%[Gmail]/Sent:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 65
        (wl-folder-is-group nil wl-folder-entity-id 76))
      . "Sent")
     (#("%[Gmail]/Draft:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 66
        (wl-folder-is-group nil wl-folder-entity-id 77))
      . "Draft")
     (#("%[Gmail]/All E-Mails:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 72
        (wl-folder-is-group nil wl-folder-entity-id 78))
      . "All E-Mails")
     (#("%Org-Mode:\"dave.abrahams@gmail.com\"/clear@imap.gmail.com:993!" 0 61
        (wl-folder-is-group nil wl-folder-entity-id 79))
      . "Org-Mode")
     (#("%[Gmail]/Draft" 0 14
        (wl-folder-is-group nil wl-folder-entity-id 4))
      . "Drafts")
     (#("%[Gmail]/Star" 0 13
        (wl-folder-is-group nil wl-folder-entity-id 2))
      . "Flagged")
     ("%Trash" . "Trash")
     (#("%INBOX" 0 6
        (wl-folder-entity-id 1 wl-folder-is-group nil))
      . "Inbox")
     (#("%[Gmail]/Starred" 0 16
        (wl-folder-entity-id 2 wl-folder-is-group nil))
      . "Important")
     (#("%[Gmail]/Sent Mail" 0 18
        (wl-folder-entity-id 3 wl-folder-is-group nil))
      . "Sent")
     (#("%[Gmail]/Drafts" 0 15
        (wl-folder-entity-id 4 wl-folder-is-group nil))
      . "Drafts")
     (#("%[Gmail]/All Mail" 0 17
        (wl-folder-entity-id 6 wl-folder-is-group nil))
      . "Archive")
     (#("%[Gmail]/Trash" 0 14
        (wl-folder-entity-id 7 wl-folder-is-group nil))
      . "Trash")
     (#("%[Gmail]/Spam" 0 13
        (wl-folder-entity-id 9 wl-folder-is-group nil))
      . "Spam")
     (#("+draft" 0 6
        (wl-folder-entity-id 10 wl-folder-is-group nil))
      . "Drafts"))))
 '(wl-folder-process-duplicates-alist
   (quote
    (("^.*" . hide)))
   nil nil "
Don't show me any duplicate messages")
 '(wl-folder-window-width 60)
 '(wl-highlight-folder-by-numbers 1)
 '(wl-highlight-x-face-function
   (quote wl-gravatar-insert))
 '(wl-icon-directory "~/.emacs.d/el-get/wanderlust/icons/")
 '(wl-interactive-exit nil)
 '(wl-interactive-save-folders nil)
 '(wl-interactive-send nil)
 '(wl-message-ignored-field-list
   (quote
    ("^.*:")))
 '(wl-message-sort-field-list
   (quote
    ("^From" "^Organization:" "^X-Attribution:" "^Subject" "^Date" "^To" "^Cc")))
 '(wl-message-visible-field-list
   (quote
    ("^\\(To\\|Cc\\):" "^Subject:" "^\\(From\\|Reply-To\\):" "^Organization:" "^Message-Id:" "^\\(Posted\\|Date\\):" "^\\(Mailer\\|User-Agent\\):" "^\\(List-Post\\):" "^\\(Xref\\):")))
 '(wl-smtp-authenticate-type "plain")
 '(wl-smtp-connection-type
   (quote starttls))
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
 '(wl-use-folder-petname
   (quote
    (modeline ask-folder read-folder))
   nil nil "
I have some hard-to-type folder names; why struggle?
")
 '(wl-user-mail-address-list
   (quote
    ("dave@boostpro.com" "dave.abrahams@gmail.com" "daveabrahams@gmail.com" "boost.consulting@gmail.com" "david.abrahams@rcn.com" "dave@luannocracy.com"))))
