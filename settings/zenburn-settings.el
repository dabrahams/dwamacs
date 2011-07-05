;; see https://github.com/dbrock/zenburn-el/issues/6
(defadvice color-theme-zenburn (after dwa/brown-cursor activate)
  (set-face-background 'cursor "brown"))
