(require 'initsplit)

;; Load up any customization themes based on the system-type and the
;; system-name.  This allows us to use the customize interface (via
;; customize-create-theme) to set up platform- and system- dependent
;; customizations.  To create a set of customizations that applies
;; when (eq system-type 'darwin), just create a theme called
;; "system-type-darwin".  See
;; [[info:emacs:Custom%20Themes][info:emacs:Custom Themes]] for more on
;; themes.
(dolist (x '(type name))
  (let* ((var-name (concat "system-" (symbol-name x))) 
         (var-value (eval (intern var-name)))
         (theme (intern (concat var-name "-" (format "%s" var-value)))))
    (ignore-errors (load-theme theme))))
