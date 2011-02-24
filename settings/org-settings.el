(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(calendar-latitude 40.845112)
 '(calendar-longitude -74.287672)
 '(calendar-mark-holidays-flag t)
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-agenda-auto-exclude-function (quote org-my-auto-exclude-function))
 '(org-agenda-cmp-user-defined (quote org-cmp-ceg-bugs))
 '(org-agenda-custom-commands (quote (("E" "Errands (next 3 days)" tags "Errand&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&STYLE<>\"habit\"&SCHEDULED<\"<+3d>\"" ((org-agenda-overriding-header "Errands (next 3 days)"))) ("A" "Priority #A tasks" agenda "" ((org-agenda-ndays 1) (org-agenda-overriding-header "Today's priority #A tasks: ") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))))) ("B" "Priority #A and #B tasks" agenda "" ((org-agenda-ndays 1) (org-agenda-overriding-header "Today's priority #A and #B tasks: ") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote regexp) "\\=.*\\[#C\\]"))))) ("w" "Waiting/delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\"" ((org-agenda-overriding-header "Waiting/delegated tasks:") (org-agenda-sorting-strategy (quote (todo-state-up priority-down category-up))))) ("u" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}" ((org-agenda-overriding-header "Unscheduled tasks: ") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote timestamp) (quote regexp) "\\* \\(DEFERRED\\|SOMEDAY\\)"))) (org-agenda-files (quote ("~/Dropbox/todo.txt"))) (org-agenda-sorting-strategy (quote (todo-state-up priority-down category-up))))) ("U" "Deferred tasks" tags "TODO=\"DEFERRED\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}" ((org-agenda-overriding-header "Deferred tasks:"))) ("S" "Someday tasks" tags "TODO=\"SOMEDAY\"&CATEGORY<>{CEG\\|ABC\\|Bizcard\\|Adagio\\|EVAprint\\|\\<IT\\>}" ((org-agenda-overriding-header "Someday tasks:"))) ("G" "Ledger tasks (all)" tags-todo "TODO<>{SOMEDAY}" ((org-agenda-files (quote ("~/src/ledger/plan/TODO"))) (org-agenda-overriding-header "Ledger tasks:") (org-agenda-sorting-strategy (quote (todo-state-up priority-down category-up))))) ("l" "Ledger tasks" tags-todo "TODO<>{SOMEDAY\\|DEFERRED}" ((org-agenda-files (quote ("~/src/ledger/plan/TODO"))) (org-agenda-overriding-header "Ledger tasks:") (org-agenda-sorting-strategy (quote (todo-state-up priority-down category-up))) (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote regexp) "\\=.*\\[#C\\]"))))) ("L" "Ledger tasks not in Bugzilla" alltodo "" ((org-agenda-files (quote ("~/src/ledger/plan/TODO"))) (org-agenda-overriding-header "Ledger tasks:") (org-agenda-sorting-strategy (quote (todo-state-up priority-down category-up))) (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote regexp) "\\(bug:\\)"))))) ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2" ((org-agenda-overriding-header "Uncategorized items"))) ("W" "Unscheduled work tasks" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&CATEGORY<>\"Website\"&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&TODO<>\"\"&LEVEL>1" ((org-agenda-overriding-header "Unscheduled work tasks") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote scheduled) (quote deadline)))) (org-agenda-sorting-strategy (quote (todo-state-up priority-down))))) ("z" "CEG tasks not in Bugzilla" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&CATEGORY<>{Website\\|Admin}&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"NOTE\"&TODO<>\"\"&LEVEL>1&SCOPE<>\"local\"" ((org-agenda-overriding-header "CEG tasks not in Bugzilla") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote regexp) "\\(cegbug:\\)"))))) ("Z" "CEG tasks in Bugzilla" tags "CATEGORY={CEG\\|ABC\\|Bizcard\\|Adagio\\|IT\\|EVAprint}&TODO<>\"DONE\"&TODO<>\"CANCELLED\"&TODO<>\"DELEGATED\"&TODO<>\"NOTE\"&LEVEL>1" ((org-agenda-overriding-header "CEG tasks in Bugzilla") (org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote notregexp) "cegbug:"))) (org-agenda-sorting-strategy (quote (todo-state-up category-down priority-down user-defined-up))))))))
 '(org-agenda-deadline-leaders (quote ("D: " "D%d: ")))
 '(org-agenda-deadline-relative-text "D%d: ")
 '(org-agenda-deadline-text "D: ")
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files (quote ("/Users/dave/Dropbox/todo.txt" "/Users/dave/Dropbox/Projects")))
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
 '(org-agenda-prefix-format (quote ((agenda . "  %-11:c%?-12t% s") (timeline . "  % s") (todo . "  %-11:c") (tags . "  %-11:c"))))
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy (quote ((agenda habit-down time-up todo-state-up priority-down category-keep) (todo priority-down category-keep) (tags priority-down category-keep) (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-tags-column -100)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-archive-location "TODO-archive::")
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-attach-method (quote mv))
 '(org-capture-templates (quote (("t" "Task" entry (file+headline "~/Dropbox/todo.txt" "Inbox") "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :Link: %a
  :ID:       %(shell-command-to-string \"uuidgen\")  :END:
  %U" :prepend t))))
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-persist (quote history))
 '(org-completion-use-ido t)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/todo.txt")
 '(org-directory "~/Dropbox/Projects")
 '(org-ellipsis (quote org-habit-alert-face))
 '(org-enforce-todo-dependencies t)
 '(org-extend-today-until 2)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-hide-leading-stars t)
 '(org-mac-mail-account "GMail: BoostPro")
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files (quote (org-agenda-files org-agenda-text-search-extra-files "~/Dropbox/Projects")))
 '(org-mobile-inbox-for-pull "~/Dropbox/from-mobile.org")
 '(org-modules (quote (org-crypt org-id org-habit org-mac-message org-bookmark org-eval)))
 '(org-refile-targets (quote ((org-agenda-files :level . 1) (org-agenda-files :todo . "PROJECT"))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote (("Task" 116 "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :ID:       %(shell-command-to-string \"uuidgen\")  :END:
  %U" nil "Inbox" nil))))
 '(org-reverse-note-order t)
 '(org-speed-commands-user nil)
 '(org-stuck-projects (quote ("+LEVEL=1/-DONE" ("TODO" "STARTED" "NEXT" "NEXTACTION") nil "\\(Appointments\\|Notes\\|Anniversaries\\)")))
 '(org-tag-alist (quote ((#("LUANN" 0 5 (face nil)) . 110) (#("WORK" 0 4 (face nil)) . 119))))
 '(org-tags-column -97)
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces (quote (("TODO" :foreground "medium blue" :weight bold) ("APPT" :foreground "medium blue" :weight bold) ("NOTE" :foreground "brown" :weight bold) ("STARTED" :foreground "dark orange" :weight bold) ("WAITING" :foreground "red" :weight bold) ("DELEGATED" :foreground "dark violet" :weight bold) ("DEFERRED" :foreground "dark blue" :weight bold) ("SOMEDAY" :foreground "dark blue" :weight bold) ("PROJECT" :height 1.5 :weight bold :foreground "black"))))
 '(org-todo-keywords (quote ((sequence "TODO" "APPT" "|" "DONE" "NOTE"))))
 '(org-todo-repeat-to-state "TODO")
 '(org-use-speed-commands t)
 '(org-use-tag-inheritance t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-habit-alert-face ((((background light)) (:background "#f5f946"))))
 '(org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
 '(org-habit-clear-face ((((background light)) (:background "#8270f9"))))
 '(org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
 '(org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
 '(org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
 '(org-habit-ready-face ((((background light)) (:background "#4df946"))))
 '(org-habit-ready-future-face ((((background light)) (:background "#acfca9"))))
 '(org-mode-line-clock ((t (:inherit variable-pitch :background "green" :foreground "black" :box (:line-width -1 :style released-button) :height 1.2))) t)
 '(org-scheduled ((((class color) (min-colors 88) (background light)) nil)))
 '(org-upcoming-deadline ((((class color) (min-colors 88) (background light)) (:foreground "Brown")))))
