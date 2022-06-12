;;; $$DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gaël Chamoulaud"
      user-mail-address "gchamoul@redhat.com")

(doom-load-envvars-file "~/.doom.d/myenv")

(keychain-refresh-environment)
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Iosevka Comfy" :size 12.5 :weight 'extrabold :height 135))
;; doom-variable-pitch-font (font-spec :family "Iosevka Comfy" :size 12.5 :weight 'normal))
;;
(cond
 (IS-MAC
  (setq doom-font (font-spec :family "Agave" :size 18 :weight 'normal)
        doom-variable-pitch-font (font-spec :family "Agave" :size 18 :weight 'normal)))
 (IS-LINUX
  (setq doom-font (font-spec :family "Agave" :size 18 :weight 'normal)
        doom-variable-pitch-font (font-spec :family "Agave" :size 18 :weight 'normal))))
;; doom-variable-pitch-font (font-spec :family "Agave" :size 14 :weight 'normal))

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(map! :nv "C-=" #'er/contract-region
      :nv "C-+" #'er/expand-region)

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.el" doom-private-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)

(map! :leader
      (:prefix-map ("g" . "gc-prefix")
       :desc "Editing my work dot org file"        "w" (cmd! (find-file
                                                              "~/Dropbox/org/work.org"))
       :desc "Editing my personal dot org file"    "p" (cmd! (find-file
                                                              "~/Dropbox/org/personal.org"))
       :desc "Editing my .yabairc file"            "y" (cmd! (find-file
                                                              "~/.yabairc"))
       :desc "Editing my .skhdrc file"             "s" (cmd! (find-file
                                                              "~/.skhdrc"))
       :desc "Editing my neomutt.rc file"          "n" (cmd! (find-file
                                                              "~/.muttrc"))
       :desc "Display World Clock"                 "h" #'world-clock

       :desc "Search for matching line"            "l" #'consult-line
       :desc "Jump to flycheck error"              "f" #'consult-flycheck

       ))

;; Store my bookmarks in my private directory
(setq bookmark-default-file (expand-file-name "private/bookmarks" doom-private-dir))

;; (setq evil-mode-line-format nil
;;       evil-insert-state-cursor '(bar "Gold1")
;;       evil-visual-state-cursor '(box "#F86155")
;;       evil-normal-state-cursor '(box "DeepSkyBlue3"))

(cond (IS-MAC (menu-bar-mode t)))

(solaire-global-mode t)
(setq all-the-icons-scale-factor 1.1)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(misc-info debug lsp minor-modes checker input-method indent-info
                buffer-encoding major-mode process checker vcs "    ")) ; <-- added padding here
  (setq
   doom-modeline-hud nil
   doom-modeline-bar-width 3
   doom-modeline-major-mode-color-icon t
   doom-modeline-major-mode-icon t
   doom-modeline-lsp t
   doom-modeline-buffer-file-name-style 'truncate-all
   doom-modeline-display-default-persp-name t
   doom-modeline-persp-name t
   doom-modeline-gnus nil
   doom-modeline-mu4e nil
   doom-modeline-github nil
   doom-modeline-minor-modes nil
   doom-modeline-window-width-limit fill-column
   doom-modeline-modal-icon t)
  )

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

(setq which-key-idle-delay 0.5)
(setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))

;; Using Red Hat First logo as a banner! ;-)
(setq fancy-splash-image (expand-file-name "red_hat_logo.png" doom-private-dir))
;; ;; Don't need the menu; I know them all by heart
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; show the filepath in the frame title
;; http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

;;Line Spacing by default
(setq-default line-spacing 0)
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      ;; evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; Use ido and flx-ido instead of ivy
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

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


;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; LSP configuration
(setq lsp-enable-file-watchers nil)

(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.local/bin")

;; (setq +ivy-buffer-preview t)

;; (after! ivy
;;   ;; I prefer search matching to be ordered; it's more precise
;;   (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; (after! company
;;   (set-company-backend! 'org-mode '(company-yasnippet company-capf company-files company-elisp company-ispell))
;;   (setq company-idle-delay 0.25
;;         company-minimum-prefix-length 2)
;;   (setq-default history-length 1000)
;;   (setq-default prescient-history-length 1000)
;;   ;; (add-to-list 'company-backends '(company-capf company-files company-yasnippet))
;;   ;; (add-to-list '+lsp-company-backends 'company-files)
;;   )

(rainbow-mode 1)

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
       (:map dired-mode-map
        :leader
        :desc "Peep-dired image previews"
        "d p" #'peep-dired
        :leader
        :desc "Dired view file"
        "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
;; (evil-define-key 'normal dired-mode-map
;;   (kbd "h") 'dired-up-directory
;;   (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; ;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
;; (evil-define-key 'normal peep-dired-mode-map
;;   (kbd "j") 'peep-dired-next-file
;;   (kbd "k") 'peep-dired-prev-file)
;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "feh")
                              ("jpg" . "feh")
                              ("png" . "feh")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:()

(require 'modus-themes)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-region '(bg-only no-extend))
(setq modus-themes-diffs 'desaturated)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-mode-line '(moody accented borderless))
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-markup '(intense background bold italic))
(setq modus-themes-deuteranopia t)
(setq modus-themes-completions
      '((matches extrabold background intense)
        (selection semibold accented intense)
        (popup accented)))
(setq modus-themes-links '(faint italic bold background))
(setq modus-themes-prompts '(intense background italic bold))
(setq modus-themes-org-agenda
      '((header-block . (variable-pitch 1.5))
        (header-date . (grayscale workaholic bold-today 1.2))
        (event . (accented italic varied))
        (scheduled . uniform)
        (habit . traffic-light)))
;; (setq modus-themes-intense-hl-line t)
(setq modus-themes-subtle-line-numbers t)
(setq modus-themes-lang-checkers '(background text-also straight-underline))
(setq modus-themes-hl-line '(intense accented))
(setq modus-themes-headings
      '((1 . (background overline variable-pitch 1.5))
        (2 . (background overline rainbow 1.3))
        (3 . (overline 1.1))
        (t . (monochrome))))
(setq modus-themes-scale-headings t)
(setq modus-themes-syntax '(faint alt-syntax yellow-comments green-strings))
(setq modus-themes-paren-match '(intense bold))
(setq modus-themes-variable-pitch-headings t)
(setq modus-themes-variable-pitch-ui nil)
(setq modus-themes-region '(accented bg-only no-extend))

;; Load the theme files before enabling a theme
(modus-themes-load-themes)

;; Load the theme of your choice:
(modus-themes-load-vivendi)

(global-anzu-mode +1)
(global-set-key (kbd "C-;") 'iedit-mode)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

(yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.doom.d/snippets/"))
(setq yas-triggers-in-field t)

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

;; `org-indent-mode' does not play nice with git-gutter, so let's disable it
(setq git-gutter:disabled-modes '(org-mode))
(global-git-gutter+-mode 1)
;; colorized dired https://github.com/purcell/diredfl
(diredfl-global-mode t)

(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
(add-hook 'text-mode-hook #'abbrev-mode)
(setq-default save-abbrevs 'silent)
(setq-default abbrev-file-name "~/.doom.d/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; (cua-mode nil)
;; ;; Switch to the new window after splitting
;; (setq evil-split-window-below t
;;       evil-vsplit-window-right t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-noter-notes-search-path '("~/Dropbox/org/noter/"))

;; Disable line-numbers for org and text modes
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
(add-hook 'text-mode-hook #'doom-disable-line-numbers-h)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default
 display-line-numbers-current-absolute nil        ;; Current line is 0
 display-line-numbers-type 'relative              ;; Prefer relative numbers
 display-line-numbers-width 2                     ;; Enforce width to reduce computation
 )

(setq truncate-lines 1)
(map! :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(setq-default
 ad-redefinition-action 'accept                   ;; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ;; Hide the cursor in inactive windows
 display-time-default-load-average nil            ;; Don't display load average
 fill-column 80                                   ;; Set width for automatic line breaks
 help-window-select t                             ;; Focus new help windows when opened
 inhibit-startup-screen t                         ;; Disable start-up screen
 ;; initial-scratch-message ""                    ;; Empty the initial *scratch* buffer
 kill-ring-max 128                                ;; Maximum length of kill ring
 load-prefer-newer t                              ;; Prefers the newest version of a file
 mark-ring-max 128                                ;; Maximum length of mark ring
 scroll-conservatively most-positive-fixnum       ;; Always scroll by one line
 select-enable-clipboard t                        ;; Merge system's and Emacs' clipboard
 tab-width 4                                      ;; Set width for tabs
 vc-follow-symlinks t                             ;; Always follow the symlinks
 view-read-only t)                                ;; Always open read-only buffers in view-mode
;; (cd "~/")                                         ;; Move to the user directory
(column-number-mode 1)                            ;; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ;; Replace yes/no prompts with y/n
(global-hl-line-mode 1)                           ;; Hightlight current line
(set-default-coding-systems 'utf-8)               ;; Default to utf-8 encoding
(show-paren-mode 1)                               ;; Show the parent
(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(setq-default auto-fill-function 'do-auto-fill)

(global-visual-fill-column-mode 0)

(global-git-commit-mode t)
(setq git-commit-style-convention-checks
      '(non-empty-second-line
        overlong-summary-line))

;; (setq ghub-use-workaround-for-emacs-bug 'force)
(setq url-privacy-level '(email))
(setq auth-sources '("/Users/gchamoul/.authinfo"))
(after! magit
  (setq magit-diff-refine-hunk 'all)
  (setq magit-save-some-buffers nil)
  (setq magit-remote-ref-format 'remote-slash-branch)
  ;; (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-commit-signoff t)

  (global-set-key (kbd "<f3>") 'magit-status)

  ;; (add-to-list 'forge-alist '("gitlab.cee.redhat.com" "gitlab.cee.redhat.com" forge-gitlab-repository))
  ;; (setq code-review-gitlab-host "gitlab.cee.redhat.com/api")
  ;; (setq code-review-gitlab-baseurl "gitlab.cee.redhat.com")
  ;; (setq code-review-gitlab-graphql-host "gitlab.cee.redhat.com")
  (setq
   forge-alist
   '(("gitlab.cee.redhat.com" "gitlab.cee.redhat.com/api/v4" "gitlab.cee.redhat.com" forge-gitlab-repository)
     ("github.com" "api.github.com" "github.com" forge-github-repository))
   )

  (setq code-review-gitlab-host "gitlab.cee.redhat.com/api")
  (setq code-review-gitlab-graphql-host "gitlab.cee.redhat.com/api")
  (setq magit-repository-directories
        '(("~/Projects/Code/tripleo/UPSTREAM" . 2)
          ("~/Projects/Code/tripleo/OOOQ/" . 2)
          ("~/Projects/Code/Insights/github/" . 2)
          ("~/Projects/Code/Insights/gitlab/" . 2)
          ("~/Projects/Code/laptop_config/" . 2)
          ("~/Projects/Code/ansible/" . 2))))

(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0))) ; Disable it
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(setq git-commit-summary-max-length 80)
(add-to-list 'git-commit-known-pseudo-headers "Co-Authored-By")
(add-to-list 'git-commit-known-pseudo-headers "Related-Bug")
(add-to-list 'git-commit-known-pseudo-headers "Resolves")
(add-to-list 'git-commit-known-pseudo-headers "Closes-Bug")
(add-to-list 'git-commit-known-pseudo-headers "Implements")
(add-to-list 'git-commit-known-pseudo-headers "Change-Id")
(add-to-list 'git-commit-known-pseudo-headers "Depends-On")
(add-to-list 'git-commit-known-pseudo-headers "Needed-By")

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

;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")

;; (define-key evil-normal-state-map (kbd "Q") (kbd "gqip"))
(setq sentence-end-double-space t)

(setq ibuffer-modified-char ?✍)
(setq ibuffer-read-only-char ?✗)
(setq ibuffer-marked-char ?✓)
(setq ibuffer-deletion-char ?␡)
(setq ibuffer-show-empty-filter-groups nil)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq org-journal-file-type 'yearly)
(setq org-journal-file-format "%Y")
(setq org-journal-date-format "%A, %m/%d/%Y")
(setq org-journal-enable-agenda-integration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs text rendering optimizations
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

;; Only render text left to right
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable Bidirectional Parentheses Algorithm
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;; Files with known long lines
;; SPC f l to open files literally to disable most text processing

;; So long mode when Emacs thinks a file would affect performance
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))

;; End of: Emacs text rendering optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/syl20bnr/spacemacs/issues/2705
;; (setq tramp-mode nil)
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(display-time-mode 1)
(setq display-time-format ".::. %a %e %b %H:%M .::.")
(setq display-time-interval 60)
(beacon-mode 1)

;; ;; Window numbering should start from "1" for each new frame.
(setq-default winum-scope 'frame-local)

(setq calendar-week-start-day 1) ; Weeks start on monday
(setq calendar-date-style 'european)
(setq european-calendar-style t)

(after! org-roam
  (setq org-roam-directory "~/Dropbox/org/roam"))

(setq calendar-legal-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed 5 1 "Fête du Travail")
        (holiday-fixed 5 8 "Armistice 1945")
        (holiday-fixed 7 14 "Fête National")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 11 "Armistice 1918")
        (holiday-fixed 12 25 "Christmas")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 50 "Lundi de Pentecôte")))

(setq calendar-celebration-holidays
      '((holiday-fixed 2 2 "Chandeleur")
        (holiday-fixed 2 14 "Fête des amoureux")
        (holiday-float 3 0 1 "Fête des grands-mères")
        (holiday-fixed 3 17 "St. Patrick's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-float 5 0 -1 "Fête des Mères")
        (holiday-float 6 0 3 "Fête des Pères")
        (holiday-fixed 6 21 "Fête de la musique")
        (holiday-fixed 10 31 "Halloween")
        (holiday-easter-etc -47 "Mardi Gras")))

(setq calendar-holidays
      `(,@holiday-solar-holidays
        ,@calendar-legal-holidays
        ,@calendar-celebration-holidays))

(setq org-hide-emphasis-markers t)
(add-to-list 'org-modules 'org-habit)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(setq org-todo-keywords
      '((sequence
         "TODO(t!)"  ; A task that needs doing & is ready to do
         "PROJ(p!)"  ; A project, which usually contains other tasks
         "STRT(s!)"  ; A task that is in progress
         "WAIT(w!)"  ; Something external is holding up this task
         "HOLD(h!)"  ; This task is paused/on hold because of me
         "|"
         "DONE(d!)"  ; Task successfully completed
         "KILL(k@)") ; Task was cancelled, aborted or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")) ; Task was completed
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)))

(with-eval-after-load 'org
  (setq org-clock-sound "~/Downloads/TibetanBowl-1.wav")
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "LawnGreen" :weight bold :bold t))
          ("STRT" . (:foreground "DarkOrange" :weight bold :bold t))
          ("WAIT" . (:foreground "firebrick3" :weight bold :bold t))
          ("HOLD" . (:foreground "cyan2" :weight bold :bold t))
          ("DONE" . (:foreground "red1"
                     :weight bold
                     :bold t
                     :underline t))
          ("KILL" . (:foreground "red3"
                     :weight bold
                     :bold t
                     :strike-through t))
          ("PROJ" . (:foreground "SlateBlue"
                     :weight bold
                     :bold t
                     :strike-through t)))))

(setq org-clock-persist-query-save t)
(setq org-clock-in-switch-to-state "STRT")
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-src-preserve-indentation t)
(setq org-enforce-todo-dependencies t)
(setq org-link-abbrev-alist
      '(("colissimo"   . "http://www.coliposte.net/particulier/suivi_particulier.jsp?colispart=")
        ("launchpad"   . "https://bugs.launchpad.net/bugs/")
        ("review"      . "https://review.opendev.org/#/c/")
        ("rhbz"        . "https://bugzilla.redhat.com/show_bug.cgi?id=")
        ("JIRA"        . "https://issues.redhat.com/browse/")
        ("github"      . "https://github.com/%s")
        ("youtube"     . "https://youtube.com/watch?v=%s")
        ("google"      . "https://google.com/search?q=")
        ("gimages"     . "https://google.com/images?q=%s")
        ("gmap"        . "https://maps.google.com/maps?q=%s")
        ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
        ("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
        ("wolfram"     . "https://wolframalpha.com/input/?i=%s")
        ("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s")
        ))

(after! org-agenda
  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-overriding-header " Agenda")
                     (org-agenda-start-day (org-today))
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header " Work")
                   (org-agenda-files '("~/Dropbox/org/work.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                                                        'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Personal")
                   (org-agenda-files '("~/Dropbox/org/personal.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                                                        'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Ideas & Goals")
                   (org-agenda-files '("~/Dropbox/org/ideas-goals.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                                                        'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Habits")
                   (org-agenda-files '("~/Dropbox/org/habits.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Projects")
                   (org-agenda-files '("~/Dropbox/org/projects.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                                                        'scheduled))))
            (todo "TODO"
                  ((org-agenda-files '("~/Dropbox/org/reading.org"))
                   (org-agenda-overriding-header " To Read")))
            ))
          ("h" "Daily habits"
           ((agenda ""))
           ((org-agenda-show-log t)
            (org-agenda-ndays 7)
            (org-agenda-log-mode-items '(state))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
          )))

(after! org (setq org-habit-show-habits t))

;; (after! evil-org
;;   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! org (setq org-log-into-drawer "LOGBOOK"
                  org-log-done 'time
                  org-log-repeat 'time
                  org-log-redeadline 'note
                  org-log-reschedule 'note))

(after! org (setq org-use-property-inheritance t))

(setq org-ellipsis " ↴ ")

;; (after! org
;;   (setq org-re-reveal-root "file:///Users/gchamoul/.doom.d/private/reveal.js/js/reveal.js"))
(setq org-tags-column 0)
;; (setq org-tag-alist '(("@home" . ?h)
;;                       ("@computer" . ?c)
;;                       ("@email" . ?e)
;;                       ("@fix" . ?f)
;;                       ("@errands")
;;                       ("@delegated")
;;                       ("@call")
;;                       ("@brainstorm")
;;                       ("@read")
;;                       ("@place")
;;                       ("someday")))

(with-eval-after-load 'org
  (setq org-capture-templates
        '(("x" "TODO PERSONAL Tasks" entry (file+headline "~/Dropbox/org/personal.org" "PERSONAL TASKS")
           "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
           :prepend t :kill-buffer t)
          ("p" "Templates for projects")
          ("pt" "Project TODO" entry (file+headline "~/Dropbox/org/projects.org" 'projectile-project-name)
           "* TODO %?\n%i\n%a" :prepend t)
          ("w" "TODO WORK Tasks" entry (file+headline "~/Dropbox/org/work.org" "WORK TASKS")
           "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
           :prepend t :kill-buffer t)
          ("i" "IDEA or GOAL" entry (file+headline "~/Dropbox/org/ideas-goals.org" "IDEAS AND GOALS")
           "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
           :prepend t :kill-buffer t)
          ("h" "HABIT" entry (file+headline "~/Dropbox/org/habits.org" "HABITS")
           "\n* TODO %\\1%?\n:PROPERTIES:\n:TITLE: %^{TITLE}\n:STYLE: habit\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n" :prepend t :kill-buffer t)
          ("c" "Contacts" entry (file "~/Dropbox/org/contacts.org")
           "* %\\1%?\n:PROPERTIES:\n:NAME: %^{NAME}\n:EMAIL:\n:PHONE:\n:ALIAS:\n:NICKNAME:\n:NOTE:\n:ADDRESS:\n:BIRTHDAY:\n:END:" :prepend t :kill-buffer t)
          )))

(after! org (setq org-html-head-include-scripts t
                  org-export-with-toc t
                  org-export-with-author t
                  org-export-headline-levels 4
                  org-export-with-drawers nil
                  org-export-with-email t
                  org-export-with-footnotes t
                  org-export-with-sub-superscripts nil
                  org-export-with-latex t
                  org-export-with-section-numbers nil
                  org-export-with-properties nil
                  org-export-with-smart-quotes t
                  org-export-backends '(pdf ascii html latex odt md pandoc)))

(after! elfeed
  (setq elfeed-search-filter "@3-weeks-ago +unread"
        rmh-elfeed-org-files (list "~/.doom.d/private/elfeed.org")
        elfeed-db-directory "~/.elfeed/"
        elfeed-search-title-max-width 100
        shr-max-image-proportion 0.6)
  ;; (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  )

(after! org-tree-slide
  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "<f9>")
    'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>")
    'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "<f11>")
    'org-tree-slide-content)
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-narrowing-control-profile)
  (setq org-tree-slide-skip-done nil))

(setq ob-mermaid-cli-path "/usr/local/bin/mmdc")


(osx-browse-mode 1)
(setq shr-use-colors nil
      shr-bullet "• "
      shr-folding-mode t
      eww-search-prefix "https://duckduckgo.com/html?q="
      url-privacy-level '(email agent cookies lastloc))

(defun osx-browse-url-forwork (url &optional new-window browser focus)
  "Open URL in Brave, Vivaldi, or whatever I'm running on OS X for my day job.
The parameters, URL, NEW-WINDOW, and FOCUS are as documented in
the function, `osx-browse-url'."
  (interactive (osx-browse-interactive-form))
  (cl-callf or browser "com.google.Chrome") ;; Choices: com.apple.Safari
  (osx-browse-url url new-window browser focus))

(defun osx-browse-url-personal (url &optional new-window browser focus)
  "Open URL in Firefox for my personal surfing.
The parameters, URL, NEW-WINDOW, and FOCUS are as documented in
the function, `osx-browse-url'."
  (interactive (osx-browse-interactive-form))
  (cl-callf or browser "org.mozilla.Firefox")
  (osx-browse-url url new-window browser focus))

(setq
 ;; See: http://ergoemacs.org/emacs/emacs_set_default_browser.html
 browse-url-handlers
 '(("docs\\.google\\.com"  . osx-browse-url-personal)
   ("redhat\\.com"         . osx-browse-url-personal)
   ("opendev\\.org"        . osx-browse-url-personal)
   ("github\\.com"         . osx-browse-url-personal)
   ("duckduckgo\\.com"     . osx-browse-url-personal)
   ("."                    . osx-browse-url-personal)))

(setq world-clock-list
      '(("Pacific/Tahiti" "Tahiti")
        ("America/Vancouver" "Vancouver")
        ("America/Los_Angeles" "Los Angeles")
        ("America/New_York" "New York")
        ("America/Sao_Paulo" "Saõ Paulo")
        ("Europe/London" "London")
        ("Etc/UTC" "UTC")
        ("Europe/Paris" "Paris")
        ("Asia/Calcutta" "Calcutta")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Tokyo" "Tokyo")
        ("Australia/Brisbane" "Brisbane")
        ("Australia/Sydney" "Sydney")
        ("Pacific/Auckland" "Auckland")))

(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
              auto-mode-alist))

(after! focus
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

(typo-global-mode)


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(use-package! corfu
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode))

(use-package! cape)

;; (after! lsp-mode
;;   (lsp-completion-provider :none)
;;   (setq lsp-auto-guess-root t)
;;   (add-hook 'go-mode-hook #'lsp-deferred)
;;   (setq lsp-go-hover-kind "FullDocumentation")
;;   (defun lsp-go-install-save-hooks ()
;;     (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;     (add-hook 'before-save-hook #'lsp-organize-imports t t))
;;   (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
(use-package! lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  ;; (lsp-auto-guess-root t)
  ;; (lsp-go-hover-kind "FullDocumentation")

  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  ;; (add-hook 'go-mode-hook #'lsp-deferred)

  ;; (defun lsp-go-install-save-hooks ()
  ;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
  ;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; Optionally use the `orderless' completion style. See `+orderless-dispatch'
;; in the Consult wiki for an advanced Orderless style dispatcher.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package! orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Use dabbrev with Corfu!
(use-package! dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; A few more useful configurations...
(use-package! emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

(use-package! flycheck-vale)
;; (flycheck-vale-setup)
;; (flycheck-vale-enabled)
