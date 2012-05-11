(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"                    ;; regexp for start block
               "-->\\|</[^/>]*[^/]>"                    ;; regexp for end block

               "<!--"                                   ;; regexp for comment start. (need this??)
               sgml-skip-tag-forward
               nil)
             '(sgml-mode
               "<!--\\|<[^/>]*[^/]>"                    ;; regexp for start block
               "-->\\|</[^/>]*[^/]>"                    ;; regexp for end block

               "<!--"                                   ;; regexp for comment start. (need this??)
               nxml-forward-element
               nil))
