 '(erc-auto-query
   (quote window-noselect))
 '(erc-autoaway-message "I'm away (after %i seconds of idle-time)")
 '(erc-autoaway-mode t)
 '(erc-autojoin-channels-alist
   (quote
    (("localhost" "#twitter_jwiegley" "&bitlbee")
     ("freenode.net" "#ledger"))))
 '(erc-autojoin-mode t)
 '(erc-generate-log-file-name-function
   (quote erc-generate-log-file-name-short))
 '(erc-header-line-format nil)
 '(erc-hide-list
   (quote
    ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-keywords
   (quote
    ("wiegley" "ledger" "eshell" "The following message received")))
 '(erc-log-channels-directory "~/Library/Mail/ERC")
 '(erc-log-write-after-send t)
 '(erc-modules
   (quote
    (autoaway autojoin button completion dcc fill identd irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp spelling)))
 '(erc-nick "johnw")
 '(erc-port 6667)
 '(erc-prompt-for-nickserv-password nil)
 '(erc-replace-alist
   (quote
    (("</?FONT>" . ""))))
 '(erc-server "irc.freenode.net")
 '(erc-services-mode t)
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude-types
   (quote
    ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-track-faces-priority-list
   (quote
    (erc-error-face
     (erc-nick-default-face erc-current-nick-face)
     erc-current-nick-face erc-keyword-face
     (erc-nick-default-face erc-pal-face)
     erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-user-full-name
   (quote user-full-name))

;;;_ + erc

(defun erc-tiny-frame ()
  (interactive)
  (with-selected-frame
      (make-frame '((width                . 80)
                    (height               . 22)
                    (left-fringe          . 0)
                    (right-fringe         . 0)
                    (vertical-scroll-bars . nil)
                    (unsplittable         . t)
                    (has-modeline-p       . nil)
                    (background-color     . "grey80")
                    (minibuffer           . nil)))
    (switch-to-buffer "#emacs")
    (set (make-local-variable 'mode-line-format) nil)))

(defun irc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "johnw" :password
       (cdr (assoc "johnw" (cadr (assq 'freenode erc-nickserv-passwords))))))

(defun im ()
  (interactive)
  (erc :server "localhost" :port 6667 :nick "johnw" :password
       (cdr (assoc "johnw" (cadr (assq 'BitlBee erc-nickserv-passwords))))))

(defvar growlnotify-command (executable-find "growlnotify")
   "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl (concat "ERC: " (buffer-name (current-buffer)))
           message)))

(add-hook 'erc-text-matched-hook 'my-erc-hook)

