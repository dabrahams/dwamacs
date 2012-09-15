(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-check-command "hlint")
 '(haskell-config-use-unicode-symbols nil)
 '(haskell-doc-use-inf-haskell t)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation turn-on-font-lock turn-on-eldoc-mode turn-on-haskell-doc-mode turn-on-haskell-decl-scan my-haskell-mode-hook)))
 '(haskell-program-name "ghci")
 '(haskell-saved-check-command "~/.cabal/bin/hlint" t))

(require 'haskell-config)
