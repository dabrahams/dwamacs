(autoload 
  'gnus-goto-article
  "gnus")

(load (elhome-path-join elhome-directory "wiegleymacs" ".org.el"))

(defun jump-to-org-agenda ()
  (interactive)
  (unless (featurep 'org-agenda)
    (load ".org"))
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (when (called-interactively-p 'any)
              (select-window wind)
              (org-fit-window-to-buffer))
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-daylight-time-zone-name "CDT")
 '(calendar-latitude 40.845112)
 '(calendar-longitude -74.287672)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "CST")
 '(calendar-time-zone -420)
 '(diary-file "~/Documents/Tasks/diary")
 '(org-M-RET-may-split-line
   (quote
    ((headline)
     (default . t))))
 '(org-agenda-auto-exclude-function
   (quote org-my-auto-exclude-function))
 '(org-agenda-custom-commands
   (quote
    (("E" "Errands (next 3 days)" tags "Errand&TODO<>\"DONE\"&TODO<>\"CANCELED\"&STYLE<>\"habit\"&SCHEDULED<\"<+3d>\""
      ((org-agenda-overriding-header "Errands (next 3 days)")))
     ("A" "Priority #A tasks" agenda ""
      ((org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's priority #A tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote notregexp)
          "\\=.*\\[#A\\]")))))
     ("b" "Priority #A and #B tasks" agenda ""
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
     ("u" "Unscheduled tasks" tags "AREA<>\"Work\"&TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-files
        (quote
         ("~/Documents/Tasks/todo.txt")))
       (org-agenda-overriding-header "Unscheduled tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote timestamp)
          (quote regexp)
          "\\* \\(DEFERRED\\|SOMEDAY\\)")))
       (org-agenda-sorting-strategy
        (quote
         (priority-down)))))
     ("U" "Deferred tasks" tags "TODO=\"DEFERRED\""
      ((org-agenda-files
        (quote
         ("~/Documents/Tasks/todo.txt")))
       (org-agenda-overriding-header "Deferred tasks:")))
     ("Y" "Someday tasks" tags "TODO=\"SOMEDAY\""
      ((org-agenda-overriding-header "Someday tasks:")))
     ("G" "Ledger tasks (all)" alltodo ""
      ((org-agenda-files
        (quote
         ("~/src/ledger/plan/TODO")))
       (org-agenda-overriding-header "Ledger tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("N" "Ledger tasks (all, alphabetical)" alltodo ""
      ((org-agenda-files
        (quote
         ("~/src/ledger/plan/TODO")))
       (org-agenda-overriding-header "Ledger tasks, alphabetical:")
       (org-agenda-sorting-strategy
        (quote
         (alpha-up)))))
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
     ("L" "Ledger tasks not in Bugzilla" tags "TODO<>{DONE\\|TESTED\\|CLOSED\\|NOTE}&LEVEL=2"
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
          "#")))))
     ("r" "Uncategorized items" tags "CATEGORY=\"Inbox\"&LEVEL=2"
      ((org-agenda-overriding-header "Uncategorized items")))
     ("V" "Unscheduled work-related tasks" tags "AREA=\"Work\"&TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Unscheduled work-related tasks")
       (org-agenda-files
        (quote
         ("~/Documents/Tasks/todo.txt")))
       (org-agenda-sorting-strategy
        (quote
         (category-up)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote scheduled)
          (quote deadline)
          (quote timestamp)
          (quote regexp)
          "\\* \\(DEFERRED\\|SOMEDAY\\)")))))
     ("W" "Work-related tasks" tags "AREA=\"Work\"&TODO<>\"\"&TODO<>{DONE\\|CANCELED\\|NOTE\\|PROJECT}"
      ((org-agenda-overriding-header "Work-related tasks")
       (org-agenda-files
        (quote
         ("~/Documents/Tasks/todo.txt")))
       (org-agenda-sorting-strategy
        (quote
         (category-up priority-down todo-state-up alpha-up)))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\* \\(DEFERRED\\|SOMEDAY\\)"))))))))
 '(org-agenda-deadline-leaders
   (quote
    ("D: " "D%d: ")))
 '(org-agenda-deadline-relative-text "D%d: ")
 '(org-agenda-deadline-text "D: ")
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files
   (quote
    ("~/Documents/Tasks/todo.txt" "~/src/ledger/plan/TODO")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
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
      (file+headline "~/Documents/Tasks/todo.txt" "Inbox")
      "* TODO %?
  SCHEDULED: %t
  :PROPERTIES:
  :ID:       %(shell-command-to-string \"uuidgen\")  :CREATED:  %U
  :END:" :prepend t))))
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
 '(org-crypt-disable-auto-save nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Documents/Tasks/todo.txt")
 '(org-directory "~/Documents/Tasks/")
 '(org-drawers
   (quote
    ("PROPERTIES" "CLOCK" "LOGBOOK" "OUT")))
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-export-babel-evaluate nil)
 '(org-extend-today-until 8)
 '(org-fast-tag-selection-single-key
   (quote expert))
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-hide-leading-stars t)
 '(org-id-locations-file "~/.emacs.d/data/org-id-locations")
 '(org-irc-link-to-logs t t)
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files nil)
 '(org-mobile-inbox-for-pull "~/Documents/Tasks/from-mobile.org")
 '(org-modules
   (quote
    (org-id org-info org-habit)))
 '(org-refile-targets
   (quote
    (("~/Documents/Tasks/todo.txt" :level . 1)
     ("~/Documents/Tasks/todo.txt" :todo . "PROJECT")
     ("~/src/ledger/plan/TODO" :level . 1))))
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-tags-column -97)
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "medium blue" :weight bold)
     ("APPT" :foreground "medium blue" :weight bold)
     ("NOTE" :foreground "brown" :weight bold)
     ("STARTED" :foreground "dark orange" :weight bold)
     ("WAITING" :foreground "red" :weight bold)
     ("DELEGATED" :foreground "dark violet" :weight bold)
     ("DEFERRED" :foreground "dark blue" :weight bold)
     ("SOMEDAY" :foreground "dark blue" :weight bold)
     ("PROJECT" :foreground "#088e8e" :weight bold))))
 '(org-todo-repeat-to-state "TODO")
 '(org-use-property-inheritance
   (quote
    ("AREA")))
 '(org-use-speed-commands t)
 '(org-x-backends
   (quote
    (ox-org ox-redmine)))
 '(org-x-redmine-title-prefix-function
   (quote org-x-redmine-title-prefix))
 '(org-x-redmine-title-prefix-match-function
   (quote org-x-redmine-title-prefix-match)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-habit-alert-face
   ((((background light))
     (:background "#f5f946"))))
 '(org-habit-alert-future-face
   ((((background light))
     (:background "#fafca9"))))
 '(org-habit-clear-face
   ((((background light))
     (:background "#8270f9"))))
 '(org-habit-clear-future-face
   ((((background light))
     (:background "#d6e4fc"))))
 '(org-habit-overdue-face
   ((((background light))
     (:background "#f9372d"))))
 '(org-habit-overdue-future-face
   ((((background light))
     (:background "#fc9590"))))
 '(org-habit-ready-face
   ((((background light))
     (:background "#4df946"))))
 '(org-habit-ready-future-face
   ((((background light))
     (:background "#acfca9"))))
 '(org-scheduled
   ((((class color)
      (min-colors 88)
      (background light))
     nil)))
 '(org-upcoming-deadline
   ((((class color)
      (min-colors 88)
      (background light))
     (:foreground "Brown")))))
