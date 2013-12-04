;; If point is in a .cxx file, bring up the .h file of the same name,
;; and vice versa.  If there's an .ixx or a .ipp file of the same base
;; name, that goes in the cycle after the .h file
(defun my-cpp-toggle-src-hdr (&optional arg)
  "If point is in a .cpp file, switch to its corresponding .h file, or a .ixx/.ipp file if one exists.
If point is in a .ixx/.ipp file, switch to its corresponding .h file, or a .cpp file if one exists.
If point is in a .h file, switch to its corresponding .cpp file."
  (interactive "p")
  (let ((this-file (buffer-file-name)))
    ;; find this file suffix
    (setq point (string-match "\\.[CcHhIi][XxPp]?[XxPp]?$" this-file))
    (if point
        (progn
          (let*((suffix (substring this-file point))
                (prefix (substring this-file 0 point))
                
                ;; construct candidate suffix patterns
                (new-suffix-patterns
                 (cond
                  ((string-match "^\\.[Cc]" suffix) '(".[hH]*" ".[iI][XxPp][XxPp]"))
                  ((string-match "^\\.[Hh]" suffix) '(".[iI][XxPp][XxPp]" ".[cC]*"))
                  ((string-match "^\\.[Ii]" suffix) '(".[cC]*"  ".[hH]*"))))

                ;; find another file to open
                (other-file
                 
                 (my-first-non-nil
                  new-suffix-patterns
                  
                  (lambda (sufpat)
                    (let ((expanded
                           (car (file-expand-wildcards (concat prefix sufpat)))))
                      ;; if an expansion was found and the file exists, return it
                      (and expanded (file-exists-p expanded) expanded)
                      )
                    )
                  
                  )
                 ))
            
            (if other-file (find-file other-file)))))))


(provide 'dwa-c++)
