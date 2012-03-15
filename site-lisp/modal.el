(defun my-mark-or-point ()
  "Return the mark if it is active, otherwise the point."
  (if
      (if (fboundp 'region-active-p) (region-active-p) mark-active)
      (mark)
    (point)))

(defun my-selection ()
  "Return a pair [start . finish) delimiting the current selection"
      (let ((start (make-marker))
            (finish (make-marker)))
        (set-marker start (min (my-mark-or-point) (point)))

        (set-marker finish (max (my-mark-or-point) (point)))
        (cons start finish)))

(defun my-replace-in-region (start finish key replacement)
  "In the range [START, FINISH), replace text matching KEY with REPLACEMENT"
  (goto-char start)
  (while (search-forward key finish t)
    (replace-match replacement)))

(defun my-activate-mark ()
  "Make the mark active if it is currently inactive"
  (set-mark (mark t)))

; return the first non-nil result of applying f to each element of seq
(defun my-first-non-nil (seq f)
  (and seq
       (or
        (apply f (list (car seq)))
        (my-first-non-nil (cdr seq) f)))
  )

(defun my-mode-read ()
  (let ((symb 'c++-mode)
	(predicate 'commandp)
	(enable-recursive-minibuffers t)
	val)
    (setq val (completing-read
	       (concat "Mode "
		       (if symb
			   (format " (default %s)" symb))
		       ": ")
	       obarray predicate t nil))
    (list (if (equal val "")
	      symb
	    (intern val)))))

(defun my-clone-region-set-mode (&optional mode)
  (interactive (my-mode-read))
  (let ((pt (point))(mk (my-mark-or-point)))
    (with-current-buffer (clone-indirect-buffer-other-window "*clone*" t)
    (narrow-to-region pt mk)
    (if mode
	(funcall mode)
      (lisp-mode)))))

(provide 'modal)
