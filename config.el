;;; $$DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+defaults")
(load! "+ui")
(load! "+themes")
(load! "+text")
(load! "+git")
(load! "+lsp")
(load! "+news")
(load! "+internet")
(load! "+emoji")
(load! "+dired")
(load! "+xkcd")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gaël Chamoulaud"
      user-mail-address "gchamoul@redhat.com")

(setq url-privacy-level '(email))
(setq auth-sources '("/Users/gchamoul/.authinfo")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

(doom-load-envvars-file "~/.doom.d/myenv")

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 100 :side right)
                    ("^\\*Man.*" :size 100 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(add-hook 'yaml-mode-hook (lambda () (ansible 1)))
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(custom-set-variables
 '(flycheck-python-flake8-executable "/usr/local/bin/python")
 '(flycheck-python-pycompile-executable "/usr/local/bin/python")
 '(flycheck-python-pylint-executable "/usr/local/bin/python"))

;; Enable projectile caching because doom doesn't enable by default
(setq projectile-enable-caching t)
(setq projectile-project-search-path '("~/Projects/Code"))
;; (setq projectile-completion-system 'ivy)
(setq projectile-mode-line
      '(:eval (format " Projectile[%s]"
                      (projectile-project-name))))
(setq python-shell-interpreter "/usr/local/bin/python")
(setq python-shell-interpreter-args "")

(after! focus
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

(typo-global-mode)

;; (use-package! flycheck-vale)
;; (flycheck-vale-setup)
;; (flycheck-vale-enabled)
;; Split indirect buffer, via
;; https://github.com/frankjonen/emacs-for-writers/blob/898c9755598c8e689019b751f194e2e73b23bb03/.spacemacs
