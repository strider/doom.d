;;; +lsp.el -*- lexical-binding: t; -*-

(lsp-ui-mode)
(lsp-ui-doc-frame-mode)
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-border t
        lsp-ui-doc-include-signature t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-imenu-enable t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.5)
  )

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--view)
(define-key lsp-ui-imenu-mode-map (kbd "RET") 'lsp-ui-imenu--view)

(define-key lsp-ui-flycheck-list-mode-map (kbd "<M-RET>") 'lsp-ui-flycheck-list--visit)
(define-key lsp-ui-flycheck-list-mode-map (kbd "RET") 'lsp-ui-flycheck-list--view)

;; LSP configuration
(setq lsp-enable-file-watchers nil)
