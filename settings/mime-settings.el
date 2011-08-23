(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mime-edit-split-message nil nil nil "
This should really be the default.  Most MUAs can't decode the split messages!")
 '(mime-play-delete-file-immediately nil)
 '(mime-save-directory "/tmp")
 '(mm-attachment-override-types
   (quote
    ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/*"))
   nil nil "
Added image/* to display attached images inline")
 '(mm-discouraged-alternatives
   (quote
    ("text/html" "text/richtext" "image/.*"))
   nil nil "
The documentation for this variable says it all")
 '(mm-inline-large-images
   (quote resize))
 '(mm-inline-text-html-with-images t)
 '(mm-text-html-renderer
   (quote w3m)))
