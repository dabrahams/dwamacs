(deftheme system-type-windows-nt
  "Created 2012-08-27.")

(custom-theme-set-variables
 'system-type-windows-nt
 '(default-frame-alist (quote ((vertical-scroll-bars) (font . "Lucida Console-11"))))
 '(exec-path 
   (let ((*nix-tools "c:/msysgit/mingw/bin;c:/msysgit/bin"))
     (setenv "PATH" (replace-regexp-in-string "/" "\\\\" *nix-tools))
     (append (split-string *nix-tools ";") exec-path))))

(provide-theme 'system-type-windows-nt)
