(deftheme dave
  "Created 2011-10-19.")

(custom-theme-set-faces 
 'dave 
 '(gnus-cite-1 ((((class
                   color) (background dark)) (:foreground "light blue")) (((class
                   color) (background
                           light)) (:foreground "MidnightBlue")) (t (:slant
                                                                     italic :inherit (dwa/mail-citation)))))
 '(show-paren-match ((t (:inherit (font-lock-keyword) :background "black" :foreground "#7cb8bb" :weight
                                  bold))))
 '(font-lock-type-face ((t (:inherit (font-lock-type) :foreground "#8cd0d3" :weight
                                     normal))))
 '(cursor ((t (:foreground "#3f3f3f" :background "#7fff00"))))
 '(mode-line ((((class color) (min-colors 88) (background
                                               light)) (:box (:line-width -1 :color nil :style
                                                                          released-button) :foreground "black" :background "grey75")) (((class
                                                                                                                                         color) (min-colors 88) (background dark)) (:box (:line-width
                                                                                                                                                                                          2 :color "#1e2320" :style
                                                                                                                                                                                          released-button) :foreground "#acbc90" :background "#1e2320"))))
 '(mode-line-inactive ((((class color) (min-colors
                                        88) (background light)) (:box (:line-width
                                                                       -1 :color "grey75" :style
                                                                       released-button) :foreground "grey20" :background "grey90")) (((class
                                                                                                                                       color) (min-colors 88) (background dark)) (:box (:line-width
                                                                                                                                                                                        2 :color "#2f3130" :style
                                                                                                                                                                                        nil) :foreground "#767e68" :background "#2f3130"))))
 '(org-level-4 ((t (:inherit (zenburn-green+3) :foreground "#d0bf8f"))))
 '(hl-line ((t (:inherit (zenburn-highlight-damp))))))

(provide-theme 'dave)
