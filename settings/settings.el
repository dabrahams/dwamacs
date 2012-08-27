(defface dwa/org-habit nil "")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("~/src/buildbot/master/docs/" "~/Library/Info/python/" "~/Library/Info/c++11/")))
 '(Info-breadcrumbs-in-header-flag t)
 '(Info-fit-frame-flag nil)
 '(Info-saved-nodes
   (quote
    ("(emacs)Top" "(elisp)Top" "(org)Top" "(gnus)Top" "(std)Top")))
 '(Man-header-file-path
   (quote
    ("/usr/include" "/usr/local/include" "/opt/local/include")))
 '(ac-clang-flags
   (quote
    ("-I" "/Users/dave/src/boost-svn-git" "-I" "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/")))
 '(ac-dictionary-directories
   (quote
    ("~/.emacs.d/elhome/ac-dict/")))
 '(ad-redefinition-action
   (quote accept)
   nil nil "The default, `warn', Makes a lot of noise for no apparent benefit")
 '(ansi-color-names-vector
   [zenburn-bg zenburn-red zenburn-green zenburn-yellow zenburn-blue zenburn-magenta zenburn-cyan zenburn-fg])
 '(auth-sources
   (quote
    (macos-keychain-internet macos-keychain-generic "~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))
 '(backup-directory-alist
   (quote
    (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil nil nil "
Blinking cursor just annoys me")
 '(byte-compile-verbose nil)
 '(c-default-style
   (quote
    ((c++-mode . "cc-mode")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))
 '(c-offsets-alist
   (quote
    ((inline-open . 0)
     (inher-intro . 2)
     (substatement-open . 0)
     (access-label . -3)
     (arglist-close . 0))))
 '(canlock-password "963afd5a40a33c7f59217100af5a7c1648af74a1")
 '(clang-flags
   (quote
    ("-I" "/Users/dave/src/boost-svn-git" "-I" "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/")))
 '(color-theme-history-max-length t)
 '(color-theme-is-cumulative nil)
 '(cursor-type
   (quote box)
   t)
 '(custom-buffer-done-kill t)
 '(custom-face-default-form
   (quote all))
 '(custom-magic-show
   (quote short))
 '(custom-magic-show-button t)
 '(custom-safe-themes
   (quote
    ("1b7caa779ad718320632954165993f48d9a0418efb59e131d53f3b952f83bde3" "0faff4eae0a8bc28f3f9b9f096b893db78ccc823f73cc34420a629b2cbc6da5d" "864cb146d904312336068803efd75fb965e8e62355e5c6f85dfe3474f51898c0" "b7419fc63a23f84014f0f29dd99a2bb883d7d58e" "a263088a00f531a10f43500d3a782ebbbe7b5092" "e7cc30a240db2b04f082c9a62db0a68d9f841a70" "0988a1666cfff8417b19384adc36d075b739b9a4" default)))
 '(custom-theme-directory "~/.emacs.d/el-get/dwamacs/settings")
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/el-get/dwamacs/settings" custom-theme-directory t)))
 '(custom-unlispify-tag-names nil)
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (font . "Monaco-14"))))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(diff-default-read-only t nil nil "
If you don't do this, all the nice navigation stuff is disabled by default.  Who wants to edit diffs by hand, anyway?")
 '(diff-jump-to-old-file t)
 '(diff-switches "-du")
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t nil nil "This customization replaces John's entire desire for sunrise,
which I now deinstall with relish")
 '(dired-listing-switches "-alh" nil nil "
Added -h so I can read file sizes")
 '(dired-use-ls-dired
   (quote unspecified))
 '(display-time-mode t)
 '(el-get-byte-compile nil)
 '(el-get-recipe-path
   (quote
    ("~/.emacs.d/el-get/dwamacs/el-get-recipes/personal/" "/Users/dave/.emacs.d/el-get/el-get/recipes" "~/.emacs.d/el-get/el-get/recipes/elpa/" "~/.emacs.d/el-get/el-get/recipes/emacswiki/")))
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
 '(gdb-delete-out-of-scope nil)
 '(gdb-many-windows t)
 '(gdb-max-frames 100 nil nil "
Increased the number of stack frames displayed from 40")
 '(gdb-show-main t)
 '(glasses-face
   (quote dwa/glasses))
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(global-auto-revert-mode t nil nil "
We want our file buffers to stay up-to-date with changes on disk")
 '(global-ede-mode t)
 '(gravatar-icon-size 50)
 '(gravatar-retrieval-program "wget -q -O '%s' '%s'" nil nil "
Requires wget, which isn't on the Mac by default.  Someday should
figure out how to use curl instead, but for now I just installed wget
from macports.")
 '(gravatar-size 48)
 '(hl-line-sticky-flag nil)
 '(ido-enable-flex-matching t)
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
     ("\\`\\(dwa/\\)?\\(org\\|calendar\\|diary\\)-" "org-settings.el" nil nil)
     ("\\`\\(dwa/\\)?\\(mime\\|mm\\)-" "mime-settings.el" nil nil)
     ("\\`\\(dwa/\\)?\\(wl\\|apel\\|flim\\|semi\\|elmo\\)-" "wl-settings.el" nil nil)
     ("\\`\\(dwa/\\)?yas/" "yasnippet-settings.el" nil nil)
     ("\\`\\(dwa/\\)?\\(nn\\|gnus-\\)" "gnus-settings.el" nil nil)
     ("\\`\\(dwa/\\)?bcc-" "byte-code-cache-settings.el" nil nil))))
 '(initsplit-pretty-print t)
 '(ipa-file-function
   (quote ipa-get-sidecar-file))
 '(irony-compiler-executable "/opt/local/bin/clang++-mp-3.1")
 '(irony-header-directories
   (quote
    ("/Users/dave/src/boost/boost-via-svn/trunk" "/opt/local/include" "/opt/local/Library/Frameworks/Python.framework/Headers")))
 '(irony-mode-line " Fe")
 '(ispell-program-name "aspell")
 '(magit-completing-read-function
   (quote magit-ido-completing-read))
 '(magit-create-branch-behaviour
   (quote at-point))
 '(magit-repo-dirs
   (quote
    ("/Users/dave/src")))
 '(magit-repo-dirs-depth 3)
 '(mairix-file-path "~/Library/Data/Indexes")
 '(mf-display-padding-height 46 nil nil "The default value of 45 leaves the titlebar tucked up under the menu bar on OSX Lion
See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=10449")
 '(mf-max-width 1920)
 '(muse-project-alist
   (quote
    (("WikiPlanner"
      ("~/plans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(next-error-highlight t)
 '(ns-alternate-modifier
   (quote control)
   nil nil "
I'm continually pressing option when I mean control.  So, I get no
Command key.  Oh, well!  I wish I could make right-command work as
command.")
 '(ns-command-modifier
   (quote meta))
 '(ns-pop-up-frames nil)
 '(ns-right-alternate-modifier
   (quote hyper))
 '(ns-right-command-modifier
   (quote super))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(pp^L-^L-string "····································✄····································
")
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
 '(python-python-command "env python")
 '(remember-annotation-functions
   (quote
    (org-remember-annotation))
   nil nil "
As prescribed by http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html.  Note: buffer-file-name was checked in the default.")
 '(remember-handler-functions
   (quote
    (org-remember-handler))
   nil nil "As prescribed by http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html.  Note: remember-append-to-file is checked in the default.")
 '(safe-local-variable-values
   (quote
    ((require-final-newline . t)
     (org-confirm-babel-evaluate)
     (org-export-babel-evaluate . t)
     (org-refile-targets
      (nil :todo . "THEME"))
     (org-refile-targets
      (nil :todo . "PROJECT"))
     (org-refile-targets
      (org-agenda-files :level . 3))
     (org-refile-targets
      (nil :tag . "refiletarget"))
     (eval add-hook
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
 '(send-mail-function
   (quote smtpmail-send-it))
 '(server-mode nil)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(shr-width nil)
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
 '(svn-program "/opt/local/bin/svn")
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(tool-bar-mode nil nil nil "
Tool bars take up valuable screen real-estate for icons whose meaning I forget")
 '(truncate-partial-width-windows nil)
 '(user-mail-address "dave@boostpro.com")
 '(vc-diff-switches "-du")
 '(vc-handled-backends
   (quote
    (Git)))
 '(w3m-confirm-leaving-secure-page t nil nil "
I never like being nannied by regular browsers either.")
 '(w3m-default-display-inline-images t)
 '(w3m-display-ins-del nil)
 '(w3m-fill-column -50 nil nil "
When I use variable-pitch-mode the text tends to run off the right 
side of the window.  This drastic setting was what I needed to prevent that.")
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
 '(wg-morph-on nil)
 '(which-function-mode t)
 '(workgroups-mode t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold-italic
   ((t
     (:inherit
      (bold italic)))))
 '(diff-refine-change
   ((((background light))
     (:background "#FFFFC0"))))
 '(dwa/glasses
   ((t
     (:underline "red" :weight bold))))
 '(font-lock-string-face
   ((((class color)
      (min-colors 88)
      (background light))
     (:background "Beige" :foreground "DarkGreen" :slant italic))))
 '(lazy-highlight
   ((t
     (:background "paleturquoise"))))
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
 '(wg-brace-face
   ((((class color)
      (background light))
     (:foreground "#7b8f01"))
    (((class color)
      (background dark))
     (:foreground "light slate blue"))))
 '(wg-command-face
   ((((class color)
      (background light))
     (:foreground "#80002c"))
    (((class color)
      (background dark))
     (:foreground "aquamarine"))))
 '(wg-current-workgroup-face
   ((((class color)
      (background light))
     (:foreground "dark blue"))
    (((class color)
      (background dark))
     (:foreground "white"))))
 '(wg-divider-face
   ((((class color)
      (background light))
     (:foreground "#7b8f01"))
    (((class color)
      (background dark))
     (:foreground "light slate blue"))))
 '(wg-filename-face
   ((((class color)
      (background light))
     (:foreground "#783106"))
    (((class color))
     (:foreground "light sky blue"))))
 '(wg-frame-face
   ((((class color)
      (background light))
     (:foreground "black"))
    (((class color)
      (background dark))
     (:foreground "white"))))
 '(wg-message-face
   ((((class color)
      (background light))
     (:foreground "#783106"))
    (((class color)
      (background dark))
     (:foreground "light sky blue"))))
 '(wg-mode-line-face
   ((((class color)
      (background light))
     (:foreground "#783106"))
    (((class color)
      (background dark))
     (:foreground "light sky blue"))))
 '(wg-other-workgroup-face
   ((((class color)
      (background light))
     (:foreground "#887767"))
    (((class color)
      (background dark))
     (:foreground "light slate grey"))))
 '(wg-previous-workgroup-face
   ((((class color)
      (background light))
     (:foreground "#783106"))
    (((class color)
      (background dark))
     (:foreground "light sky blue")))))
