;; Load environment variables and set up PATH from ~/.MacOSX/environment.plist
;; 
;; It was my understanding that it was supposed to happen
;; automatically, but this works, at least.
(when (and
       (eq system-type 'darwin)
       ;; This is not important enough to abort startup on failure
       (require 'osx-plist nil 'noerror))
  (osx-plist-update-environment))
