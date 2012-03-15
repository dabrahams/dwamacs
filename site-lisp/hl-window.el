(defadvice other-window (around no-empty-stanzas (count &optional all-frames)
                                        activate compile preactivate)
  "Delete empty customization stanzas for variables.  Also
remember the state of the buffer before custom-save-variables was
invoked so we can avoid writing it when there's been no real
modification."

  (let* ((prev-buffer (current-buffer)))

    ad-do-it
    
    (unless (eq prev-buffer (current-buffer))
      (with-current-buffer prev-buffer
        (when (bound-and-true-p hl-window-cookies)
          (dolist (cookie hl-window-cookies)
            (face-remap-remove-relative cookie)))
        (set (make-local-variable 'hl-window-cookies) nil))

      (set (make-local-variable 'hl-window-cookies) 
           (list
            (face-remap-add-relative 'border :background "#333333")
            (face-remap-add-relative 'default :background "#333333"))))))
