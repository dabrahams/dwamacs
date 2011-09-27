(deftheme zenburn-overrides
  "Created 2011-09-14.")

(custom-theme-set-faces
 'zenburn-overrides
 '(mode-line ((((background dark)) (:box (:line-width 2 :color "#1e2320" :style released-button) :foreground "#acbc90" :background "#1e2320"))))
 '(mode-line-inactive ((((background dark)) (:box (:line-width 2 :color "#2f3130" :style nil) :foreground "#767e68" :background "#2f3130"))))
 '(org-level-4 ((t (:inherit (zenburn-green+3))))))

(provide-theme 'zenburn-overrides)
