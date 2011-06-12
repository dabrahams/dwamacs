(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-clang-flags
   (quote
    ("-I" "/Users/dave/src/boost-svn-git" "-I" "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/")))
 '(ac-dictionary-directories
   (quote
    ("~/.emacs.d/elhome/ac-dict/")))
 '(backup-directory-alist
   (quote
    (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil nil nil "
Blinking cursor just annoys me")
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(calendar-latitude 40.845112)
 '(calendar-longitude -74.287672)
 '(calendar-mark-holidays-flag t)
 '(canlock-password "963afd5a40a33c7f59217100af5a7c1648af74a1")
 '(clang-flags
   (quote
    ("-I" "/Users/dave/src/boost-svn-git" "-I" "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/")))
 '(cursor-type
   (quote box)
   t)
 '(custom-theme-directory "~/.emacs.d/el-get/dwamacs/settings")
 '(default-frame-alist
    (quote
     ((menu-bar-lines . 1))))
 '(delete-selection-mode t nil nil "
Creates normal editor behavior: select a region and begin
typing, the region is replaced")
 '(diff-default-read-only t nil nil "
If you don't do this, all the nice navigation stuff is disabled by default.  Who wants to edit diffs by hand, anyway?")
 '(diff-jump-to-old-file t)
 '(diff-switches "-du")
 '(dired-listing-switches "-alh" nil nil "
Added -h so I can read file sizes")
 '(el-get-byte-compile nil)
 '(el-get-standard-packages
   (quote
    ("semi" "flim" "wanderlust" "apel" "yasnippet" "maxframe" "markdown-mode" "php-mode" "psvn" "nognus" "org-mode" "gravatar" "wl-gravatar" "filladapt" "emacs-w3m" "elhome" "byte-code-cache" "el-get" "browse-kill-ring" "magit" "el-get" "initsplit")))
 '(elscreen-display-screen-number t)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-kill-screen nil)
 '(erc-default-sound "~/erc.wav")
 '(erc-modules
   (quote
    (autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring smiley sound stamp track)))
 '(erc-nick "bewst")
 '(erc-notify-mode t)
 '(erc-sound-mode t)
 '(explicit-bash-args
   (quote
    ("--noediting" "-i" "-l"))
   nil nil "
added -l so it would take things out of my .bash_profile, like (on boostpro.com) the prompt pattern.  Otherwise I get this abomination: ///bd5882fff11dd5c2900e1ce95b895e66")
 '(ffap-machine-p-known
   (quote reject)
   nil nil "
This hung emacs on my Mac once when pinging.")
 '(ffap-require-prefix t nil nil "
Invoking ffap without any prefix tends to do things I don't intend.")
 '(g-user-email "dave@boostpro.com")
 '(gdb-max-frames 100 nil nil "
Increased the number of stack frames displayed from 40")
 '(global-auto-revert-mode t nil nil "
We want our file buffers to stay up-to-date with changes on disk")
 '(gravatar-icon-size 50)
 '(gravatar-retrieval-program "wget -q -O '%s' '%s'" nil nil "
Requires wget, which isn't on the Mac by default.  Someday should
figure out how to use curl instead, but for now I just installed wget
from macports.")
 '(gravatar-size 48)
 '(ido-everywhere t)
 '(ido-mode
   (quote buffer)
   nil
   (ido))
 '(ido-use-filename-at-point
   (quote guess))
 '(ido-use-url-at-point t)
 '(imap-shell-program
   (quote
    ("dovecot --exec-mail imap")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(org\\|calendar\\|diary\\)-" "org-settings.el" nil nil)
     ("\\`\\(wl\\|apel\\|flim\\|semi\\|mime\\|mm\\|elmo\\)-" "wl-settings.el" nil nil)
     ("\\`yas/" "yasnippet-settings.el" nil nil))))
 '(initsplit-pretty-print t)
 '(ispell-program-name "aspell")
 '(magit-repo-dirs
   (quote
    ("/Users/dave/src" "/Users/dave/work/pipsync")))
 '(magit-repo-dirs-depth 1)
 '(mail-dont-reply-to-names "dave@\\(boost-consulting\\|boostpro\\)\\.com\\|dave\\.abrahams@rcn\\.com\\|boost\\.consulting@gmail\\.com\\|dave\\.boostpro@gmail\\.com\\|Undisclosed-recipients[:;]*")
 '(mail-signature t)
 '(mairix-command "ssh boostpro.com mairix")
 '(mairix-file-path "/home/dave/Maildir/")
 '(mairix-mail-program
   (quote wl))
 '(mairix-search-file ".zz_mairix-results")
 '(markdown-command "markdown-extra")
 '(muse-project-alist
   (quote
    (("WikiPlanner"
      ("~/plans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(ns-alternate-modifier
   (quote control)
   nil nil "
I'm continually pressing option when I mean control.  So, I get no
Command key.  Oh, well!  I wish I could make right-command work as
command.")
 '(ns-command-modifier
   (quote meta))
 '(org-M-RET-may-split-line
   (quote
    ((headline)
     (default . t))))
 '(org-agenda-auto-exclude-function
   (quote org-my-auto-exclude-function))
 '(org-agenda-cmp-user-defined
   (quote org-cmp-ceg-bugs))
 '(org-agenda-custom-commands
   (quote
    (("E" "Errands (next 3 days)" tags "Errand&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&STYLE<>\"habit\"&SCHEDULED<\"<+3d>\""
      ((org-agenda-overriding-header "Errands (next 3 days)")))
     ("A" "Priority #A tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notregexp)
          "\\=.*\\[#A\\]")))))
     ("B" "Priority #A and #B tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A and #B tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\=.*\\[#C\\]")))))
     ("w" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
      ((org-agenda-overriding-header "Waiting/delegated tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}"
      ((org-agenda-overriding-header "Unscheduled tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote timestamp)
          (quote regexp)
          "\\* \\(DEFERRED\\|SOMEDAY\\)")))
       (org-agenda-files
        (quote
         ("~/Dropbox/todo.txt")))
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("U" "Deferred tasks" tags "TODO=\"DEFERRED\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}"
      ((org-agenda-overriding-header "Deferred tasks:")))
     ("S" "Someday tasks" tags "TODO=\"SOMEDAY\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}"
      ((org-agenda-overriding-header "Someday tasks:")))
     ("G" "Ledger tasks (all)" tags-todo "TODO<>{SOMEDAY}"
      ((org-agenda-files
        (quote
         ("~/src/ledger/plan/TODO")))
       (org-agenda-overriding-header "Ledger tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("l" "Ledger tasks" tags-todo "TODO<>{SOMEDAY\\|DEFERRED}"
      ((org-agenda-files
        (quote
         ("~/src/ledger/plan/TODO")))
       (org-agenda-overriding-header "Ledger tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\=.*\\[#C\\]")))))
     ("L" "Ledger tasks not in Bugzilla" alltodo ""
      ((org-agenda-files
        (quote
         ("~/src/ledger/plan/TODO")))
       (org-agenda-overriding-header "Ledger tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\(bug:\\)")))))
     ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
      ((org-agenda-overriding-header "Uncategorized items")))
     ("W" "Unscheduled work tasks" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&CATEGORY<>\"Website\"&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&TODO<>\"\"&LEVEL>1"
      ((org-agenda-overriding-header "Unscheduled work tasks")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline))))
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down)))))
     ("z" "CEG tasks not in Bugzilla" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&CATEGORY<>{Website\\|Admin}&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&TODO<>\"\"&LEVEL>1&SCOPE<>\"local\""
      ((org-agenda-overriding-header "CEG tasks not in Bugzilla")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\(cegbug:\\)")))))
     ("Z" "CEG tasks in Bugzilla" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"DELEGATED\"&TODO<>\"NOTE\"&LEVEL>1"
      ((org-agenda-overriding-header "CEG tasks in Bugzilla")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notregexp)
          "cegbug:")))
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up category-down priority-down user-defined-up))))))))
 '(org-agenda-deadline-leaders
   (quote
    ("D: " "D%d: ")))
 '(org-agenda-deadline-relative-text "D%d: ")
 '(org-agenda-deadline-text "D: ")
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files
   (quote
    ("/Users/dave/Dropbox/todo.txt" "/Users/dave/Dropbox/Projects")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-menu-show-matcher nil nil nil "
Wiegleymacs has some pretty long matcher strings
")
 '(org-agenda-menu-two-column t nil nil "
Wiegleymacs has some pretty long matchers
")
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11:c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11:c")
     (tags . "  %-11:c"))))
 '(org-agenda-scheduled-leaders
   (quote
    ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-tags-column -100)
 '(org-agenda-text-search-extra-files
   (quote
    (agenda-archives)))
 '(org-archive-location "TODO-archive::")
 '(org-archive-save-context-info
   (quote
    (time category itags)))
 '(org-attach-method
   (quote mv))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file+headline "~/Dropbox/todo.txt" "Inbox")
      "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :Link: %a
  :ID:       %(shell-command-to-string \"uuidgen\")  :END:
  %U" :prepend t))))
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-modeline-total
   (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-persist
   (quote history))
 '(org-completion-use-ido t)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/todo.txt")
 '(org-directory "~/Dropbox/Projects")
 '(org-ellipsis
   (quote org-habit-alert-face))
 '(org-enforce-todo-dependencies t)
 '(org-extend-today-until 2)
 '(org-fast-tag-selection-single-key
   (quote expert))
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-hide-leading-stars t)
 '(org-mac-mail-account "GMail: BoostPro")
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files
   (quote
    (org-agenda-files org-agenda-text-search-extra-files "~/Dropbox/Projects")))
 '(org-mobile-inbox-for-pull "~/Dropbox/from-mobile.org")
 '(org-modules
   (quote
    (org-id org-habit org-mac-message org-bookmark org-eval)))
 '(ps-font-family
   (quote Helvetica))
 '(ps-font-info-database
   (quote
    ((Courier
      (fonts
       (normal . "Courier")
       (bold . "Courier-Bold")
       (italic . "Courier-Oblique")
       (bold-italic . "Courier-BoldOblique"))
      (size . 10.0)
      (line-height . 10.55)
      (space-width . 6.0)
      (avg-char-width . 6.0))
     (Helvetica
      (fonts
       (normal . "Helvetica")
       (bold . "Helvetica-Bold")
       (italic . "Helvetica-Oblique")
       (bold-italic . "Helvetica-BoldOblique"))
      (size . 10.0)
      (line-height . 11.56)
      (space-width . 2.78)
      (avg-char-width . 5.09243))
     (Times
      (fonts
       (normal . "Times-Roman")
       (bold . "Times-Bold")
       (italic . "Times-Italic")
       (bold-italic . "Times-BoldItalic"))
      (size . 10.0)
      (line-height . 11.0)
      (space-width . 2.5)
      (avg-char-width . 4.71432))
     (Palatino
      (fonts
       (normal . "Palatino-Roman")
       (bold . "Palatino-Bold")
       (italic . "Palatino-Italic")
       (bold-italic . "Palatino-BoldItalic"))
      (size . 10.0)
      (line-height . 12.1)
      (space-width . 2.5)
      (avg-char-width . 5.08676))
     (Helvetica-Narrow
      (fonts
       (normal . "Helvetica-Narrow")
       (bold . "Helvetica-Narrow-Bold")
       (italic . "Helvetica-Narrow-Oblique")
       (bold-italic . "Helvetica-Narrow-BoldOblique"))
      (size . 10.0)
      (line-height . 11.56)
      (space-width . 2.2796)
      (avg-char-width . 4.17579))
     (NewCenturySchlbk
      (fonts
       (normal . "NewCenturySchlbk-Roman")
       (bold . "NewCenturySchlbk-Bold")
       (italic . "NewCenturySchlbk-Italic")
       (bold-italic . "NewCenturySchlbk-BoldItalic"))
      (size . 10.0)
      (line-height . 12.15)
      (space-width . 2.78)
      (avg-char-width . 5.31162))
     (AvantGarde-Book
      (fonts
       (normal . "AvantGarde-Book")
       (italic . "AvantGarde-BookOblique"))
      (size . 10.0)
      (line-height . 11.77)
      (space-width . 2.77)
      (avg-char-width . 5.45189))
     (AvantGarde-Demi
      (fonts
       (normal . "AvantGarde-Demi")
       (italic . "AvantGarde-DemiOblique"))
      (size . 10.0)
      (line-height . 12.72)
      (space-width . 2.8)
      (avg-char-width . 5.51351))
     (Bookman-Demi
      (fonts
       (normal . "Bookman-Demi")
       (italic . "Bookman-DemiItalic"))
      (size . 10.0)
      (line-height . 11.77)
      (space-width . 3.4)
      (avg-char-width . 6.05946))
     (Bookman-Light
      (fonts
       (normal . "Bookman-Light")
       (italic . "Bookman-LightItalic"))
      (size . 10.0)
      (line-height . 11.79)
      (space-width . 3.2)
      (avg-char-width . 5.67027))
     (Symbol
      (fonts
       (normal . "Symbol"))
      (size . 10.0)
      (line-height . 13.03)
      (space-width . 2.5)
      (avg-char-width . 3.24324))
     (Zapf-Dingbats
      (fonts
       (normal . "Zapf-Dingbats"))
      (size . 10.0)
      (line-height . 9.63)
      (space-width . 2.78)
      (avg-char-width . 2.78))
     (ZapfChancery-MediumItalic
      (fonts
       (normal . "ZapfChancery-MediumItalic"))
      (size . 10.0)
      (line-height . 11.45)
      (space-width . 2.2)
      (avg-char-width . 4.10811))
     (Zapf-Chancery-MediumItalic
      (fonts
       (normal . "ZapfChancery-MediumItalic"))
      (size . 10.0)
      (line-height . 11.45)
      (space-width . 2.2)
      (avg-char-width . 4.10811)))))
 '(remember-annotation-functions
   (quote
    (org-remember-annotation))
   nil nil "
As prescribed by http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html.  Note: buffer-file-name was checked in the default.")
 '(remember-handler-functions
   (quote
    (org-remember-handler))
   nil nil "As prescribed by http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html.  Note: remember-append-to-file is checked in the default.")
 '(rmail-dont-reply-to-names "dave@\\(boost-consulting\\|boostpro\\)\\.com\\|dave\\.abrahams@rcn\\.com\\|boost\\.consulting@gmail\\.com\\|dave\\.boostpro@gmail\\.com\\|Undisclosed-recipients[:;]*")
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote write-file-hooks)
           (quote time-stamp))
     (test-case-name . buildbot\.test\.test_sourcestamp)
     (test-case-name . buildbot\.test\.test_changes)
     (test-case-name . buildbot\.broken_test\.test_web_status_json)
     (encoding . utf8)
     (folded-file . t))))
 '(server-mode t nil nil "
Always run a server so we can open files in existing emacs frames.")
 '(show-paren-mode t)
 '(smtp-server "smtp.gmail.com")
 '(smtpmail-default-smtp-server "www.boostpro.com")
 '(smtpmail-local-domain "boostpro.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-starttls-credentials
   (quote
    (("www.boostpro.com" 587 "" ""))))
 '(split-height-threshold nil)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil nil nil "
Tool bars take up valuable screen real-estate for icons whose meaning I forget")
 '(tramp-backup-directory-alist
   (quote
    (("." . "~/.emacs.d/backups"))))
 '(tramp-default-host "localhost")
 '(tramp-default-proxies-alist
   (quote
    (("\\`localhost\\'" nil nil)
     ("\\`206.217.198.21\\'" nil nil)
     ("\\`.+\\'" "\\`root\\'" "/ssh:%h:")))
   nil nil "
Gets around the common setting that prohibits ssh login as root.

Don't do any proxying for connections to localhost (depends
on the customization of tramp-default-host to \"localhost\" for simple
matching), and otherwise, if sudo'ing somewhere, ssh there first and
then sudo on the remote host itself.")
 '(tramp-remote-path
   (quote
    (tramp-default-remote-path "/usr/sbin" "/usr/local/sbin" "/usr/local/bin" "/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")))
 '(truncate-partial-width-windows nil)
 '(user-mail-address "dave@boostpro.com")
 '(vc-diff-switches "-du")
 '(w3m-confirm-leaving-secure-page t nil nil "
I never like being nannied by regular browsers either.")
 '(w3m-default-display-inline-images t)
 '(w3m-display-ins-del nil)
 '(w3m-use-cookies t)
 '(warning-suppress-types
   (quote
    ((\(undo\ discard-info\))))
   nil nil "
Without this, emacs pops up annoying warnings in, e.g., *shell* buffers
where I don't expect it to be keeping undo history anyway")
 '(weblogger-config-alist
   (quote
    (("homepage" "http://daveabrahams.com/xmlrpc.php" "dave" "" "2")
     ("techarcana" "http://techarcana.net/xmlrpc.php" "dave" "" "1")
     ("cpp-next" "http://cpp-next.com/xmlrpc.php" "dave" "" "1")
     ("ryppl" "http://ryppl.org/xmlrpc.php" "dave" "" "4")
     ("boostpro" "http://boostpro.com/xmlrpc.php" "dave" "" "1"))))
 '(weblogger-edit-entry-hook
   (quote
    ((lambda nil
       (switch-to-buffer "*weblogger-entry*")))))
 '(weblogger-edit-mode
   (quote my-weblogger-markdown-mode))
 '(weblogger-server-url "http://cpp-next.com/xmlrpc.php")
 '(weblogger-server-username "dave")
 '(weblogger-start-edit-entry-hook
   (quote
    ((lambda nil
       (message-goto-body)
       (while
           (search-forward "
" nil t)
         (replace-match "" nil t))))))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor
   ((default
      (:background "brown"))
    (nil nil)))
 '(diff-refine-change
   ((default nil)
    (nil
     (:background "#FFFFC0"))))
 '(elscreen-tab-background-face
   ((((class color))
     (:background "lightgray" :height 1.0 :family "Helvetica"))))
 '(font-lock-string-face
   ((((class color)
      (min-colors 88)
      (background light))
     (:background "Beige" :foreground "DarkGreen" :slant italic))))
 '(italic
   ((t
     (:slant italic :family "Monaco"))))
 '(mode-line
   ((((class color)
      (min-colors 88))
     (:inherit variable-pitch :background "lightblue" :foreground "black" :box
               (:line-width -1 :style released-button)
               :height 1.2))))
 '(rst-level-1-face
   ((t
     (:background "grey85" :foreground "black")))
   t)
 '(rst-level-2-face
   ((t
     (:inherit nil :background "grey78" :foreground "black")))
   t)
 '(rst-level-3-face
   ((t
     (:background "grey71" :foreground "black")))
   t)
 '(rst-level-4-face
   ((t
     (:background "grey64" :foreground "black")))
   t)
 '(rst-level-5-face
   ((t
     (:background "grey57" :foreground "black")))
   t)
 '(rst-level-6-face
   ((t
     (:background "grey50" :foreground "black")))
   t))
