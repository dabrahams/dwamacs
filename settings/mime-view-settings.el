;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From [[http://thread.gmane.org/gmane.mail.wanderlust.general.japanese/8021/focus=8026][Kazuhiro Ito]]
;;
;; This code makes it display a text/html entity when an alternate
;; text/plain entity does not exist
(autoload 'mime-w3m-preview-text/html "mime-w3m")

(ctree-set-calist-strictly
 'mime-preview-condition
 '((type . text)
   (subtype . html)
   (body . visible)
   (body-presentation-method . mime-w3m-preview-text/html)))

(set-alist 'mime-view-type-subtype-score-alist
           '(text . html) 3)

(set-alist 'mime-view-type-subtype-score-alist
           '(text . plain) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From [[http://mid.gmane.org/82wrozj05t.wl%25kzhr@d1.dion.ne.jp][Kazuhiro Ito]]
;;
;; This code allows us to demote "garbage" plain text that just tells
;; us to look at the HTML part of an email.
;;
(defun mime-entity-text/plain-score (entity)
  (let ((content (decode-mime-charset-string
                  (mime-entity-content entity)
                  (or (mime-content-type-parameter
                       (mime-entity-content-type entity)
                       "charset")
                      default-mime-charset)
                  'CRLF)))
    ;; Modify as you like.
    (if (and (< (length content) 160)
             (string-match "[hH][tT][mM][lL]"
                           content))
        1 4)))

(defun mime-display-multipart/alternative (entity situation)
  (let* ((children (mime-entity-children entity))
         (original-major-mode-cell (assq 'major-mode situation))
         (default-situation
           (cdr (assq 'childrens-situation situation)))
         (i 0)
         (p 0)
         (max-score 0)
         situations)
    (if original-major-mode-cell
        (setq default-situation
              (cons original-major-mode-cell default-situation)))
    (setq situations
          (mapcar (function
                   (lambda (child)
                     (let ((situation
                            (mime-find-entity-preview-situation
                             child default-situation)))
                       (if (cdr (assq 'body-presentation-method situation))
                           (let ((score
                                  (cdr
                                   (or (assoc
                                        (cons
                                         (cdr (assq 'type situation))
                                         (cdr (assq 'subtype situation)))
                                        mime-view-type-subtype-score-alist)
                                       (assq
                                        (cdr (assq 'type situation))
                                        mime-view-type-subtype-score-alist)
                                       (assq
                                        t
                                        mime-view-type-subtype-score-alist)
                                       ))))
;; == Customization Starts Here ==
                             (when (functionp score)
                               (setq score (funcall score child)))
;; == Customization Ends Here ==
                             (if (> score max-score)
                                 (setq p i
                                       max-score score)
                               )))
                       (setq i (1+ i))
                       situation)
                     ))
                  children))
    (setq i 0)
    (while children
      (let ((child (car children))
            (situation (car situations)))
        (mime-display-entity child (if (= i p)
                                       situation
                                     (put-alist 'body 'invisible
                                                (copy-alist situation)))))
      (setq children (cdr children)
            situations (cdr situations)
            i (1+ i)))))

(set-alist 'mime-view-type-subtype-score-alist
           '(text . plain) 'mime-entity-text/plain-score)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
