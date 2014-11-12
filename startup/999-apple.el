(require 'use-package)
(require 'compile)

(add-to-list 'load-path "~/src/s/shiny/utils")

(use-package shiny-mode
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.shiny$" . shiny-mode))
	  (autoload 'shiny-mode "shiny-mode" t)))
