;;; +lsp.el -*- lexical-binding: t; -*-

(when (or (modulep! :checkers syntax +flymake)
          (not (modulep! :checkers syntax)))
  (setq lsp-diagnostics-provider :flymake))
(after! lsp-mode
  (setq
   lsp-log-io nil
   lsp-auto-guess-root t
   lsp-progress-via-spinner t
   lsp-enable-file-watchers nil
   lsp-idle-delay 0.01
   lsp-completion-enable-additional-text-edit t

   lsp-signature-render-documentation t
   lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion)
   lsp-signature-doc-lines 10

   lsp-eldoc-enable-hover t
   lsp-headerline-breadcrumb-enable nil
   lsp-modeline-code-actions-segments '(count icon name)

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil

   lsp-lens-enable t))

(when (modulep! :completion company)
  (setq +lsp-company-backends '(company-capf :with company-yasnippet)))

(after! lsp-ui
  (setq
   ;; Sideline
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-code-actions nil
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-diagnostics nil
   ;; Peek
   lsp-ui-peek-enable nil
   ;; Doc
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-delay 0.51
   lsp-ui-doc-max-width 50
   lsp-ui-doc-max-height 30
   lsp-ui-doc-include-signature t
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-header t))

;;(lsp-ui-mode)
;;(lsp-ui-doc-frame-mode)
;;(lsp-lens-enable t)
;;(after! lsp-ui
;;  (setq lsp-ui-doc-enable t
;;       lsp-ui-doc-header t
;;        lsp-ui-doc-border t
;;        lsp-ui-doc-include-signature t
;;        lsp-ui-peek-enable t
;;        lsp-ui-peek-show-directory t
;;        lsp-ui-imenu-enable t
;;        lsp-ui-imenu-auto-refresh t
;;        lsp-ui-sideline-show-diagnostics t
;;        lsp-ui-sideline-show-hover t
;;        lsp-ui-sideline-show-code-actions t
;;        lsp-ui-sideline-delay 0.5)
;;  )

;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;;(define-key lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--view)
;;(define-key lsp-ui-imenu-mode-map (kbd "RET") 'lsp-ui-imenu--view)

;;(define-key lsp-ui-flycheck-list-mode-map (kbd "<M-RET>") 'lsp-ui-flycheck-list--visit)
;;(define-key lsp-ui-flycheck-list-mode-map (kbd "RET") 'lsp-ui-flycheck-list--view)

;; LSP configuration
(setq lsp-enable-file-watchers nil)
