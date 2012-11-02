(defun boost-copyright ()
  "Return the appropriate boost copyright for the current user and year"
  (concat "Copyright " (user-full-name) " " (number-to-string (nth 5 (decode-time)))
          ". Distributed under the Boost\n\
Software License, Version 1.0. (See accompanying\n\
file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)"))
      
(defun fluid-copyright ()
  "Return the appropriate FluidObjects copyright for the current user and year"
  (concat "Copyright FluidObjects Software " (number-to-string (nth 5 (decode-time)))
          ". All rights reserved."))

(defun my-split-filename (filename)
  "split a FILENAME string into a (basename . extension) pair"
  (let* ((fname filename)

         (prefix
          (if (string-match "\\(.*\\)\\(\\..*\\)" fname)
              (substring fname 0 (match-end 1))
            fname))
         (extension
          (if (match-beginning 2)
              (substring fname (+ 1 (match-beginning 2)))
            nil)))
    (cons prefix extension)
  ))

(defun my-split-current-filename ()
  (my-split-filename (file-name-nondirectory (buffer-file-name))))

;;;###autoload
(defun my-split-path (path)
  (let ((result nil) (elt nil))
    (while (and path
                (not (equal (setq elt (file-name-nondirectory path)) "")))
      (setq result (cons elt result))
      (setq path (file-name-directory path))
      (setq path (and path (directory-file-name path))))
    result))

;;;###autoload
(defun my-path-elts ()
  (subseq (my-split-path (buffer-file-name)) 0 -1))

(defun my-code-mode-hook ()
  (font-lock-mode t)
  (show-paren-mode t)
  (local-set-key [return] 'newline-and-indent)
  (local-set-key [(control return)] 'newline)
  (local-set-key [( control ?\( )] 'my-matching-paren)
  
  ;; Try to make completion case sensitive in code buffers.
  (make-local-variable 'dabbrev-case-fold-search)
  (setq dabbrev-case-fold-search nil)
  )

;;;###autoload
(defun my-copyright (&optional copyright)
  "Insert a commented COPYRIGHT string. If COPYRIGHT
is not supplied, the boost copyright is used by default"
  (interactive)
  (let ((copy-start (point)))
      
    (insert-string (concat (or copyright
                       (or (and (my-path-elts) (boost-copyright))
                           (eval (list my-default-copyright))))
                   "\n"))
      
    (comment-region copy-start (point))))

(provide 'code-settings)
