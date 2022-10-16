;;; +news.el -*- lexical-binding: t; -*-

(after! elfeed
  (setq elfeed-search-filter "@3-weeks-ago +unread"
        rmh-elfeed-org-files (list "~/.doom.d/private/elfeed.org")
        elfeed-db-directory "~/.elfeed/"
        elfeed-search-title-max-width 100
        shr-max-image-proportion 0.6)
  ;; (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  )
