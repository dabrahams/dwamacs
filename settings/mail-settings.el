;; I like citations in email to be set off from the rest of the text
;; by a slightly different background color.  This helps immeasurably
;; when people forget to leave a blank line after citations.  see
;; gnus-settings.el and wl-settings.el, where I inherit this face.
(defface dwa/mail-citation '((((class color)
			 (background dark))
			(:background "#383838"))
		       (((class color)
			 (background light))
			(:background "#efefef")))
  "Mail citation base face.")

(provide 'mail-settings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mail-dont-reply-to-names "dave@\\(boost-consulting\\|boostpro\\)\\.com\\|dave\\.abrahams@rcn\\.com\\|boost\\.consulting@gmail\\.com\\|dave\\.boostpro@gmail\\.com\\|Undisclosed-recipients[:;]*")
 '(mail-envelope-from
   (quote header))
 '(mail-setup-with-from nil)
 '(mail-signature t)
 '(mail-source-delete-incoming t)
 '(mail-source-delete-old-incoming-confirm nil)
 '(mail-source-report-new-mail-interval 15)
 '(mail-specify-envelope-from t)
 '(mail-user-agent
   (quote gnus-user-agent)))
