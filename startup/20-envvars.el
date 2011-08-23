;; Load environment variables and set up PATH from ~/.MacOSX/environment.plist
;; 
;; It was my understanding that it was supposed to happen
;; automatically, but this works, at least.
(when (and
       (eq system-type 'darwin)
       ;; This is not important enough to abort startup on failure
       (ignore-errors (require 'osx-plist)))
  (osx-plist-update-environment))
