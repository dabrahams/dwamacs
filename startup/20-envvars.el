;; Load environment variables and set up PATH from ~/.MacOSX/environment.plist
;; 
;; It was my understanding that it was supposed to happen
;; automatically, but this works, at least.

;; This is not important enough to abort startup on failure
(eval-when-compile
  (require 'cl))

(when (and (eq system-type 'darwin)
           (require 'osx-plist nil 'noerror))
  ;; Replace the implementation of osx-plist-update-exec-path with one
  ;; that respects the ordering of $PATH and avoids duplicates
  (defun osx-plist-update-exec-path ()
    "Update `exec-path' from the PATH environment variable."
    (let ((path (delq nil (parse-colon-path (getenv "PATH")))))
      (setq exec-path
            (dolist (dir exec-path path)
              (add-to-list 'path (file-name-as-directory dir) :append)))))

  (when (file-exists-p "~/.MacOSX/Environment.plist")
    (osx-plist-update-environment)))

