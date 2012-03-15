(deftheme zenburn-overrides
  "Created 2012-03-14.")

(custom-theme-set-faces
 'zenburn-overrides
 '(show-paren-match ((((background dark)) (:background "black" :inherit (font-lock-keyword)))))
 '(font-lock-type-face ((t (:weight normal :inherit (font-lock-type)))))
 '(cursor ((t (:foreground "#3f3f3f" :background "#7fff00"))))
 '(mode-line ((((class color) (min-colors 88) (background light)) (:box (:line-width 2 :style released-button) :background "grey75" :foreground "black")) (((class color) (min-colors 88) (background dark)) (:box (:line-width 2 :color "#1e2320" :style released-button) :foreground "#acbc90" :background "#1e2320")) (t :inverse-video t)))
 '(mode-line-inactive ((default :inherit mode-line) (((class color) (min-colors 88) (background light)) (:weight light :box (:line-width 2 :color "grey75" :style nil) :foreground "grey20" :background "grey90")) (((class color) (min-colors 88) (background dark)) (:weight light :box (:line-width 2 :color "#2f3130" :style nil) :foreground "#767e68" :background "#2f3130"))))
 '(org-level-4 ((t (:inherit (zenburn-green+3)))))
 '(hl-line ((t (:inherit zenburn-highlight-damp))))
 '(font-lock-warning-face ((t (:inherit error :foreground "indian red" :weight bold)))))

(provide-theme 'zenburn-overrides)
