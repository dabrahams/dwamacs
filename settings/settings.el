(defface dwa/org-habit nil "")

;; For some reason the customization keeps getting clobbered
;;(setq w3m-command "/opt/local/bin/w3m")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("~/Library/Info/python/" "~/Library/Info/c++11/")))
 '(Info-breadcrumbs-in-header-flag t)
 '(Info-default-directory-list
   (quote
    ("/Users/xin/midway/emacs-mac-port/build/share/info/" "/Applications/Emacs.app/Contents/Resources/info" "/opt/local/share/info/" "/usr/share/info/")))
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
 '(backup-directory-alist
   (quote
    (("." . "~/.emacs.d/backups"))))
 '(blink-cursor-mode nil nil nil "
Blinking cursor just annoys me")
 '(byte-compile-verbose nil)
 '(c-backspace-function
   (quote backward-delete-char))
 '(c-default-style
   (quote
    ((c-mode . "dwa")
     (c++-mode . "dwa")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "dwa"))))
 '(canlock-password "963afd5a40a33c7f59217100af5a7c1648af74a1")
 '(clang-flags
   (quote
    ("-I" "/Users/dave/src/boost-svn-git" "-I" "/opt/local/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/")))
 '(color-theme-history-max-length t)
 '(color-theme-is-cumulative nil)
 '(column-number-mode nil)
 '(coq-unicode-tokens-enable t)
 '(create-lockfiles nil)
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
    ("484955d17f5ecf9960c88e2dc8fbff997121e2804bf6ce5cbc297314c28a050d" "b75cb5c678c1129f92140dba07155675bb26250f28df59f714707b3eb162f92b" "f10d561ff8739114cc013ad05167e56618c054fb618dc8e866675fd55955ad21" "d7e021fb20767633bbabc55caa0ba0ebf09e14d6f1e25e105b073a60922ccfb5" "4ce6246e997a7d5ccc3fe3c53d76c612513f988ef36c41dedb91f7c625d9300b" "6be0e2efb6bfcdf5df031cded2efcaeb245d4018c9798d7e9b5138e534f0736a" "ba0380b2213842f54345de57b1065deb610fcc2176ab65846c3d80ee7ca0a8f7" "b06f914a0125006cf3d4e413a42a95d77a21ef54a512b8ea683f2cc4bcd45772" "ec33995fd55b0d78405f207232c6fb86c9f101f8bac5f0f969a42bfe6bad3355" "bf330ec6af1dbb8aed2e19c07b8f5e61c63dd4fed041551717205b471c9f823a" "af2f95f810c2642aa039b5b11ddb309fbd42f5e5d6c032d68a49fad18dc0c968" "dea61a12a79fac7f0aa23784d2f11f10d351925904a27913af301fc769a436d7" "479b343f577693cb162aabfffb32c20dd78ad31eaee4e622764c55409ef38722" "a50de17cacd11e23e3f917fb47a634345937937255870e332beeddb73254e6f3" "5fa3b591d2fae9bd1a250a7ebe09c46ede99e131040754cf92a679208827d9d4" "329ee3023d4fe4408174ed5463b4f4903926d499356a5803d10bb8f47a387eb6" "26fb90f317d9788b546389ec5171f2ccd49e86735c1d1a8260100f61005104c3" "9604bae13a3a91860220c39403abb0ed2d793de836b469252c06ec8022820e92" "bb0041c0aa4680b708465c7150ff29fe1cc7a6d69b8b8fc2cd5f57a520d82f70" "2485eaead83db99165857aae3bf31dcaf184f0e286ed7d948383e4fdf8737f50" "4373a5e070ab804ffff5b2b53a12c945f9317c64a07840b8b5fb1be652604d1b" "d1e9809f97e33a8e461e5acefd4c6f54b6075bf0761e1a76c8c3fb9b0d47fe7b" "fb75d2d70e2696134e4bdad6ae10f10dd0c741bf8a2d375bbdcf864314de723e" "1b7caa779ad718320632954165993f48d9a0418efb59e131d53f3b952f83bde3" "0faff4eae0a8bc28f3f9b9f096b893db78ccc823f73cc34420a629b2cbc6da5d" "864cb146d904312336068803efd75fb965e8e62355e5c6f85dfe3474f51898c0" "b7419fc63a23f84014f0f29dd99a2bb883d7d58e" "a263088a00f531a10f43500d3a782ebbbe7b5092" "e7cc30a240db2b04f082c9a62db0a68d9f841a70" "0988a1666cfff8417b19384adc36d075b739b9a4" default)))
 '(custom-theme-directory "~/.emacs.d/el-get/dwamacs/settings")
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/el-get/dwamacs/settings" custom-theme-directory t)))
 '(custom-unlispify-tag-names nil)
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
 '(doc-view-continuous t)
 '(doc-view-resolution 300)
 '(el-get-byte-compile nil)
 '(el-get-sources
   (quote
    ((:name gtags :type http :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/global/global/gtags.el" :description "gtags facility for Emacs" :website "http://www.gnu.org/software/global/" :commands gtags-mode :diminish gtags-mode :after
            (progn
              (defun my-gtags-or-semantic-find-tag nil
                (interactive)
                (if
                    (and
                     (fboundp
                      (quote semantic-active-p))
                     (funcall
                      (function semantic-active-p)))
                    (call-interactively
                     (function semantic-complete-jump))
                  (call-interactively
                   (function gtags-find-tag))))
              (add-hook
               (quote after-change-major-mode-hook)
               (lambda nil
                 (gtags-mode t)))
              (bind-key "M-."
                        (quote my-gtags-or-semantic-find-tag)
                        gtags-mode-map)
              (bind-key "C-c t ."
                        (quote gtags-find-rtag))
              (bind-key "C-c t f"
                        (quote gtags-find-file))
              (bind-key "C-c t p"
                        (quote gtags-parse-file))
              (bind-key "C-c t g"
                        (quote gtags-find-with-grep))
              (bind-key "C-c t i"
                        (quote gtags-find-with-idutils))
              (bind-key "C-c t s"
                        (quote gtags-find-symbol))
              (bind-key "C-c t r"
                        (quote gtags-find-rtag))
              (bind-key "C-c t v"
                        (quote gtags-visit-rootdir))
              (bind-key "<mouse-2>"
                        (quote gtags-find-tag-from-here)
                        gtags-mode-map)
              (use-package helm-gtags :bind
                           ("M-T" . helm-gtags-select)
                           :config
                           (bind-key "M-,"
                                     (quote helm-gtags-resume)
                                     gtags-mode-map)))
            :symbol gtags))))
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
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t nil nil "
We want our file buffers to stay up-to-date with changes on disk")
 '(gravatar-icon-size 50)
 '(gravatar-retrieval-program "wget -q -O '%s' '%s'" nil nil "
Requires wget, which isn't on the Mac by default.  Someday should
figure out how to use curl instead, but for now I just installed wget
from macports.")
 '(gravatar-size 48)
 '(hl-line-sticky-flag nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil)
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
     ("\\`\\(dwa/\\)?yas\\(nippet\\)?\\(-\\|/\\)" "yasnippet-settings.el" nil nil)
     ("\\`\\(dwa/\\)?\\(nn\\|gnus-\\)" "gnus-settings.el" nil nil)
     ("\\`\\(dwa/\\)?bcc-" "byte-code-cache-settings.el" nil nil)
     ("\\`\\(haskell\\|ghc\\)" "haskell-mode-settings.el" nil nil))))
 '(initsplit-pretty-print t)
 '(ipa-file-function
   (quote ipa-get-sidecar-file))
 '(irony-compiler-executable "/opt/local/bin/clang++-mp-3.1")
 '(irony-header-directories
   (quote
    ("/Users/dave/src/boost/boost-via-svn/trunk" "/opt/local/include" "/opt/local/Library/Frameworks/Python.framework/Headers")))
 '(irony-mode-line " Fe")
 '(ispell-program-name "aspell")
 '(mac-option-modifier
   (quote alt))
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
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(pp^L-^L-string "····································✄····································
")
 '(proof-electric-terminator-enable t)
 '(proof-shell-fiddle-frames nil nil nil "John says, \"I was able to get the default \"three windows mode\"
for Proof General working reliably by customizing this\"")
 '(proof-splash-enable nil)
 '(proof-three-window-enable nil)
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
    ((tab-always-indent t)
     (whitespace-style face lines indentation:space)
     (whitespace-mode)
     (whitespace-style tabs)
     (whitespace-style indentation:space)
     (whitespace-style face indentation:space)
     (whitespace-style face lines indentation:tab)
     (require-final-newline . t)
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
 '(smtpmail-smtp-service 587)
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
 '(rst-adornment
   ((t
     (:inherit
      (font-lock-keyword-face fixed-pitch))))
   t)
 '(rst-directive
   ((t
     (:inherit
      (font-lock-builtin-face fixed-pitch))))
   t)
 '(rst-level-1
   ((((background light))
     (:inherit fixed-pitch :background "grey85"))
    (((background dark))
     (:inherit fixed-pitch :background "grey15")))
   t)
 '(rst-level-1-face
   ((t
     (:background "grey85" :foreground "black")))
   t)
 '(rst-level-2
   ((((background light))
     (:inherit rst-level-1 :background "grey78"))
    (((background dark))
     (:inherit rst-level-1 :background "grey22")))
   t)
 '(rst-level-2-face
   ((t
     (:inherit nil :background "grey78" :foreground "black")))
   t)
 '(rst-level-3
   ((((background light))
     (:inherit rst-level-2 :background "grey71"))
    (((background dark))
     (:inherit rst-level-2 :background "grey29")))
   t)
 '(rst-level-3-face
   ((t
     (:background "grey71" :foreground "black")))
   t)
 '(rst-level-4
   ((((background light))
     (:inherit rst-level-3 :background "grey64"))
    (((background dark))
     (:inherit rst-level-3 :background "grey36")))
   t)
 '(rst-level-4-face
   ((t
     (:background "grey64" :foreground "black")))
   t)
 '(rst-level-5
   ((((background light))
     (:inherit rst-level-4 :background "grey57"))
    (((background dark))
     (:inherit rst-level-4 :background "grey43")))
   t)
 '(rst-level-5-face
   ((t
     (:background "grey57" :foreground "black")))
   t)
 '(rst-level-6
   ((((background light))
     (:inherit rst-level-5 :background "grey50"))
    (((background dark))
     (:inherit rst-level-5 :background "grey50")))
   t)
 '(rst-level-6-face
   ((t
     (:background "grey50" :foreground "black")))
   t)
 '(rst-literal
   ((t
     (:inherit
      (font-lock-string-face fixed-pitch))))
   t)
 '(variable-pitch
   ((t
     (:family "Myriad Set"))))
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
