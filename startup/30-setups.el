(autoload 'org-magit-store-link "org-magit")
(autoload 'org-magit-open "org-magit")
(autoload 'org-magit-export "org-magit")
(add-hook 'org-store-link-functions 'org-magit-store-link)
