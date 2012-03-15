;;;###autoload
(defun winrsh ()
  "Performs settings when connecting to a Windows host."
  (interactive)
  (let ((remote-method (file-remote-p default-directory 'method)))
    (if (and remote-method (string-equal remote-method "smb"))
	(let (
              (explicit-shell-file-name "powershell")
 	      (explicit-powershell-args '("-file" "-"))

;             (explicit-shell-file-name "cmd")
;	     (explicit-cmd-args '("-q"))
              )
	  (call-interactively 'shell))
      (call-interactively 'shell))))