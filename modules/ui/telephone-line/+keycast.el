;;; ui/telephone-line/+keycast.el -*- lexical-binding: t; -*-

(use-package! keycast
  :commands keycast-mode
  :when (modulep! +keycast)
  :config
  ;; (define-minor-mode keycast-mode
  ;;   "Show current command and its key in the mode-line."
  ;;   :global t
  ;;   (if keycast-mode
  ;;       (progn
  ;;         (add-hook 'pre-command-hook 'keycast-mode-line-update t)
  ;;         (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
  ;;     (remove-hook 'pre-command-hook 'keycast-mode-line-update)
  ;;     (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))

  ;; `emacs-doom-themes' doesn't yet support `keycast' faces.
  (custom-set-faces!
    '(keycast-command :inherit mode-line-emphasis)
    '(keycast-key :inherit mode-line-highlight
                  :weight bold))

  ;; Prettier insert events.
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing...")))

  ;; Remove mouse scroll events.
  (dolist (event '(mwheel-scroll
                   mouse-event-p
                   mouse-movement-p
                   mouse-movement))
    (add-to-list 'keycast-substitute-alist `(,event nil))))
