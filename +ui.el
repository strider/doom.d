;;; +ui.el -*- lexical-binding: t; -*-

;; Make the scratch buffer start in lisp mode
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

(setq doom-font (font-spec :family "Agave" :size 18 :weight 'normal)
      doom-big-font (font-spec :family "Agave" :size 36 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 22 :weight 'light))

(defvar required-fonts '("Agave" "JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")

(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))

;; (defun +doom-dashboard-setup-modified-keymap ()
;;   (setq +doom-dashboard-mode-map (make-sparse-keymap))
;;   (map! :map +doom-dashboard-mode-map
;;         :desc "Find file" :ng "f" #'find-file
;;         :desc "Recent files" :ng "r" #'consult-recent-file
;;         :desc "Config dir" :ng "C" #'doom/open-private-config
;;         :desc "Open config.el" :ng "c" (cmd! (find-file (expand-file-name "config.el" doom-user-dir)))
;;         :desc "Open dotfile" :ng "." (cmd! (doom-project-find-file "~/.config/"))
;;         :desc "Notes (roam)" :ng "n" #'org-roam-node-find
;;         :desc "Switch buffer" :ng "b" #'+vertico/switch-workspace-buffer
;;         :desc "Switch buffers (all)" :ng "B" #'consult-buffer
;;         :desc "IBuffer" :ng "i" #'ibuffer
;;         :desc "Previous buffer" :ng "p" #'previous-buffer
;;         :desc "Set theme" :ng "t" #'consult-theme
;;         :desc "Quit" :ng "Q" #'save-buffers-kill-terminal
;;         :desc "Show keybindings" :ng "h" (cmd! (which-key-show-keymap '+doom-dashboard-mode-map))))

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "n" #'org-roam-node-find
      :ne "c" (cmd! (find-file (expand-file-name "config.el" doom-user-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "i" #'ibuffer
      :ne "q" #'save-buffers-kill-terminal)

(setq doom-fallback-buffer-name "► Doom"
      doom-fallback-buffer-name +doom-dashboard-name)

(map! :leader
      (:prefix-map ("e" . "gc-prefix")
       :desc "Editing my work dot org file"     "w" (cmd! (find-file "~/Dropbox/org/work.org"))
       :desc "Editing my personal dot org file" "p" (cmd! (find-file "~/Dropbox/org/personal.org"))
       :desc "Editing my .yabairc file"         "y" (cmd! (find-file "~/.yabairc"))
       :desc "Editing my .skhdrc file"          "s" (cmd! (find-file "~/.skhdrc"))
       :desc "Editing my neomutt.rc file"       "m" (cmd! (find-file "~/.muttrc"))
       :desc "Notes (org-roam)"                 "n" #'org-roam-node-find
       :desc "Display World Clock"              "h" #'world-clock
       :desc "Open ibuffer"                     "i" #'ibuffer
       :desc "Search for matching line"         "l" #'consult-line
       :desc "Jump to flycheck error"           "f" #'consult-flycheck
       :desc "New empty Org buffer"             "o" #'+evil-buffer-org-new
       ))

(custom-set-faces!
  '(mode-line :family "Iosevka Term SS04" :height 0.9)
  '(mode-line-inactive :family "Iosevka Term SS04" :height 0.9))

;; (after! doom-modeline
;;   (doom-modeline-def-modeline 'main
;;     '(bar matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;     '(misc-info debug lsp minor-modes checker input-method indent-info
;;       buffer-encoding major-mode process checker vcs "    ")) ; <-- added padding here
;;   (setq
;;    doom-modeline-hud t
;;    doom-modeline-bar-width 4
;;    doom-modeline-major-mode-color-icon t
;;    doom-modeline-major-mode-icon t
;;    doom-modeline-lsp t
;;    doom-modeline-buffer-file-name-style 'truncate-all
;;    doom-modeline-display-default-persp-name t
;;    doom-modeline-persp-name t
;;    doom-modeline-gnus nil
;;    doom-modeline-mu4e nil
;;    doom-modeline-github nil
;;    doom-modeline-minor-modes nil
;;    doom-modeline-window-width-limit fill-column
;;    doom-modeline-modal-icon t)
;;  )

(after! doom-modeline
  (setq all-the-icons-scale-factor 1.1
        auto-revert-check-vc-info t
        doom-modeline-hud t
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-github t
        doom-modeline-github-interval 60
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-modeline 'main
    '(bar modals workspace-name window-number persp-name buffer-position selection-info buffer-info matches remote-host debug vcs matches)
    '(github grip checker misc-info repl lsp "    ")))

(setq fancy-splash-image (expand-file-name "banner.png" doom-user-dir))

;; show the filepath in the frame title
;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
