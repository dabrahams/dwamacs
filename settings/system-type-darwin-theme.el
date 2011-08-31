(deftheme system-type-darwin
  "Created 2010-12-20.")

(when (eq system-type 'darwin)
  (add-to-list 'face-ignored-fonts "\\`-[^-]*-\\(lucida console\\|monaco\\)-[^-]*bold-"))

(custom-theme-set-variables
 'system-type-darwin
 '(default-frame-alist (quote ((menu-bar-lines . 1) (tool-bar-lines . 0) (font . "Monaco-13")))))

(provide-theme 'system-type-darwin)
