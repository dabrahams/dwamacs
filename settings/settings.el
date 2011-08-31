(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("~/src/buildbot/master/docs/" "~/Library/Info/python/")))
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
 '(byte-compile-verbose nil)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
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
     ((menu-bar-lines . 1)
      (line-spacing . 3)
      (font . "DejaVu Sans Mono-14"))))
 '(delete-selection-mode t nil nil "
Creates normal editor behavior: select a region and begin
typing, the region is replaced")
 '(diff-default-read-only t nil nil "
If you don't do this, all the nice navigation stuff is disabled by default.  Who wants to edit diffs by hand, anyway?")
 '(diff-jump-to-old-file t)
 '(diff-switches "-du")
 '(dired-dwim-target t nil nil "This customization replaces John's entire desire for sunrise, 
which I now deinstall with relish")
 '(dired-listing-switches "-alh" nil nil "
Added -h so I can read file sizes")
 '(display-time-mode t)
 '(el-get-byte-compile nil)
 '(el-get-sources
   (quote
    ((:name info+ :depends
            (fit-frame misc-fns strings thingatpt+)
            :website "http://www.emacswiki.org/emacs/InfoPlus")
     (:name rs-info :type emacsmirror :website "http://lists.gnu.org/archive/html/info-gnus-english/2006-10/msg00081.html" :description "Info enhancements from Reiner Steib, including boxquote")
     (:name org-mode :url "http://github.com/jwiegley/org-mode")
     (:name org-html5presentation :type git :url "git://gist.github.com/509761.git")
     (:name org-magit :type git :url "https://github.com/sigma/org-magit")
     (:name gnus-harvest :type git :url "git://github.com/jwiegley/gnus-harvest.git")
     (:name tramp :type cvs :url ":pserver:anonymous@cvs.savannah.gnu.org:/sources/tramp" :website "http://www.gnu.org/s/tramp/" :build
            (\`
             (("autoconf")
              ("./configure"
               (\,
                (concat "--with-emacs=" el-get-emacs))
               "--with-contrib"
               (\,
                (concat "--prefix="
                        (expand-file-name
                         (el-get-package-directory "tramp")))))
              ("make")
              ("make"
               ("install"))))
            :load-path
            ("share/emacs/site-lisp")
            :info "share/info" :module "tramp")
     (:name workspaces :type emacswiki)
     (:name company :type elpa)
     (:name grep-ed :type emacswiki)
     (:name xmlunicode :depends
            (unichars)
            :type http :url "http://nwalsh.com/emacs/xmlchars/xmlunicode.el" :post-init
            (lambda nil
              (progn
                (autoload
                  (quote unicode-character-shortcut-insert)
                  "xmlunicode" nil t)
                (autoload
                  (quote unicode-smart-double-quote)
                  "xmlunicode" nil t)
                (setq-default unicode-character-list-file
                              (concat
                               (el-get-package-directory "unichars")
                               "unichars.el")))))
     (:name unichars :type http :url "http://nwalsh.com/emacs/xmlchars/unichars.el")
     (:name boxquote :type http :url "http://www.davep.org/emacs/boxquote.el")
     (:name wiegleymacs :type git :url "http://github.com/jwiegley/dot-emacs" :load-path nil)
     (:name dwamacs :depends
            (elhome)
            :type git :url "git@github.com:dabrahams/dwamacs" :load-path
            ("." "settings" "startup" "site-lisp"))
     (:name zenburn-theme :type git :url "git@github.com:dabrahams/zenburn-theme.git" :load-path
            ("themes")
            :compile
            ("themes"))
     (:name bbatsov-zenburn :depends
            (color-theme)
            :type git :url "https://github.com/bbatsov/zenburn-emacs" :build
            (let
                ((default-directory
                   (el-get-package-directory "bbatsov-zenburn")))
              (mkdir "site-lisp")
              (copy-file "color-theme-zenburn.el" "site-lisp" t)
              nil)
            :load-path
            ("site-lisp")
            :compile
            ("site-lisp")))))
 '(el-get-standard-packages
   (quotet
    ("color-theme-zenburn" "magit" "color-theme" "semi" "flim" "wanderlust" "apel" "yasnippet" "maxframe" "markdown-mode" "php-mode" "psvn" "nognus" "org-mode" "gravatar" "wl-gravatar" "filladapt" "emacs-w3m" "elhome" "byte-code-cache" "el-get" "browse-kill-ring" "el-get" "initsplit" "wanderlust")))
 '(elscreen-buffer-list-enabled t)
 '(elscreen-buffer-to-nickname-alist
   (quote
    (("[Ss]hell" . "shell")
     ("compilation" . "compile")
     ("-telnet" . "telnet")
     ("dict" . "OnlineDict")
     ("*WL:Message*" . "Wanderlust"))))
 '(elscreen-display-screen-number nil)
 '(elscreen-display-tab t)
 '(elscreen-mode-to-nickname-alist
   (quote
    (("^dired-mode$" lambda nil
      (format "Dired(%s)" dired-directory))
     ("^Info-mode$" lambda nil
      (format "Info(%s)"
              (file-name-nondirectory Info-current-file)))
     ("^mew-draft-mode$" lambda nil
      (format "Mew(%s)"
              (buffer-name
               (current-buffer))))
     ("^mew-" . "Mew")
     ("^\\(irchat\\|erc\\)-" . "Chat")
     ("^liece-" . "Liece")
     ("^lookup-" . "Lookup")
     ("^gnus-" . "Gnus"))))
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(enable-local-eval t)
 '(eudc-inline-expansion-format
   (quote
    ("%s <%s>" name email)))
 '(explicit-bash-args
   (quote
    ("--noediting" "-i" "-l"))
   nil nil "
added -l so it would take things out of my .bash_profile, like (on boostpro.com) the prompt pattern.  Otherwise I get this abomination: ///bd5882fff11dd5c2900e1ce95b895e66")
 '(ffap-machine-p-known
   (quote accept))
 '(ffap-machine-p-local
   (quote reject))
 '(ffap-machine-p-unknown
   (quote ping))
 '(ffap-require-prefix t)
 '(font-lock-verbose nil)
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
 '(ido-gather-virtual-filenames
   (quote
    (ido-gather-recent-files ido-gather-git-project-files)))
 '(ido-mode
   (quote buffer)
   nil
   (ido))
 '(ido-use-filename-at-point
   (quote guess))
 '(ido-use-url-at-point t)
 '(ido-use-virtual-buffers t)
 '(imap-shell-program
   (quote
    ("/opt/local/sbin/dovecot --exec-mail imap" "ssh %s imapd" "rsh %s imapd" "ssh %g ssh %s imapd" "rsh %g rsh %s imapd")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(gnus\\(-home\\)?\\|message\\)-directory\\'" "preloaded-settings.el" nil t)
     ("\\`erc-nickserv-passwords\\'" "../startup/10-passwd.el" nil nil)
     ("\\`\\(org\\|calendar\\|diary\\)-" "org-settings.el" nil nil)
     ("\\`\\(mime\\|mm\\)-" "mime-settings.el" nil nil)
     ("\\`\\(wl\\|apel\\|flim\\|semi\\|elmo\\)-" "wl-settings.el" nil nil)
     ("\\`yas/" "yasnippet-settings.el" nil nil)
     ("\\`\\(nn\\|gnus-\\)" "gnus-settings.el" nil nil)
     ("\\`bcc-" "byte-code-cache-settings.el" nil nil))))
 '(initsplit-pretty-print t)
 '(ispell-program-name "aspell")
 '(line-spacing 0.25)
 '(magit-repo-dirs
   (quote
    ("/Users/dave/src" "/Users/dave/src/bbotpriv-top" "/Users/dave/src/fossbot-top")))
 '(magit-repo-dirs-depth 1)
 '(mairix-file-path "~/Library/Data/Indexes")
 '(markdown-command "markdown-extra")
 '(mm-discouraged-alternatives
   (quote
    ("application/msword" "text/richtext")))
 '(mm-text-html-renderer
   (quote w3m))
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
           (quote local-write-file-hooks)
           (lambda nil
             (save-excursion
               (delete-trailing-whitespace))))
     (whitespace-style face lines-tail)
     (eval add-hook
           (quote local-write-file-hooks)
           (quote
            (lambda nil
              (save-excursion
                (delete-trailing-whitespace)))))
     (eval add-hook
           (quote before-save-hook)
           (quote delete-trailing-whitespace))
     (eval whitespace-mode)
     (eval set-face-attribute
           (quote whitespace-line)
           nil :background "red1" :foreground "yellow" :weight
           (quote bold))
     (eval set-face-attribute
           (quote whitespace-tab)
           nil :background "red1" :foreground "yellow" :weight
           (quote bold))
     (whitespace-style face trailing lines-tail)
     (whitespace-line-column . 80)
     (eval require
           (quote whitespace))
     (eval add-hook
           (quote write-file-hooks)
           (quote time-stamp))
     (test-case-name . buildbot\.test\.test_sourcestamp)
     (test-case-name . buildbot\.test\.test_changes)
     (test-case-name . buildbot\.broken_test\.test_web_status_json)
     (encoding . utf8)
     (folded-file . t))))
 '(scroll-bar-mode nil)
 '(server-mode t nil nil "
Always run a server so we can open files in existing emacs frames.")
 '(show-paren-mode t)
 '(smime-certificate-directory "~/Library/Data/Gnus/Mail/certs/")
 '(smtp-server "smtp.gmail.com")
 '(smtpmail-default-smtp-server "www.boostpro.com")
 '(smtpmail-local-domain "boostpro.com")
 '(smtpmail-smtp-service 587 t)
 '(smtpmail-starttls-credentials
   (quote
    (("www.boostpro.com" 587 "" ""))))
 '(spam-report-gmane-use-article-number nil)
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
   ((((background light))
     (:background "#FFFFC0"))))
 '(font-lock-string-face
   ((((class color)
      (min-colors 88)
      (background light))
     (:background "Beige" :foreground "DarkGreen" :slant italic))))
 '(hl-line
   ((t
     (:inherit highlight-current-line))))
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
   t)
 '(show-paren-match
   ((t
     (:inherit font-lock-keyword :background "black")))))
