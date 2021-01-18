;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gaël Chamoulaud"
      user-mail-address "gchamoul@redhat.com")

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
(setq doom-font (font-spec :family "Agave" :size 20 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Agave" :size 20))
;; (setq doom-font (font-spec :family "Agave" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq-default
 doom-modeline-height 25
 doom-modeline-bar-width 3
 doom-modeline-major-mode-color-icon t
 doom-modeline-buffer-file-name-style 'truncate-all
 doom-modeline-display-default-persp-name nil
 doom-modeline-persp-name t
 doom-modeline-minor-modes nil
 doom-modeline-modal-icon nil
 )

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
;; (all-the-icons-ivy-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

(setq yas-snippet-dirs '("~/.doom.d/snippets/"))

;; colorized dired https://github.com/purcell/diredfl
(diredfl-global-mode t)
(setq-default indent-tabs-mode nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
(add-hook 'text-mode-hook #'abbrev-mode)
(setq-default save-abbrevs 'silent)
(setq-default abbrev-file-name "~/.doom.d/abbrev_defs")
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; (custom-set-faces!
;;   '(mode-line :family "Agave" :height 0.98)
;;   '(mode-line-inactive :family "Agave" :height 0.98))

(cua-mode t)
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq fancy-splash-image "~/Pictures/redhat-banner.png")
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
;; (setq org-ellipsis "[...] ")
(setq org-ellipsis " ▼ ")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default
 display-line-numbers-current-absolute nil        ;; Current line is 0
 display-line-numbers-type 'relative              ;; Prefer relative numbers
 display-line-numbers-width 2                     ;; Enforce width to reduce computation
 )

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
 use-package-always-ensure t                      ;; Avoid the :ensure keyword for each package
 vc-follow-symlinks t                             ;; Always follow the symlinks
 view-read-only t)                                ;; Always open read-only buffers in view-mode
;; (cd "~/")                                         ;; Move to the user directory
(column-number-mode 1)                            ;; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ;; Replace yes/no prompts with y/n
(global-hl-line-mode 1)                           ;; Hightlight current line
(set-default-coding-systems 'utf-8)               ;; Default to utf-8 encoding
(show-paren-mode 1)                               ;; Show the parent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(setq-default truncate-lines 1)
(setq-default auto-fill-function 'do-auto-fill)

(global-visual-fill-column-mode 0)

(global-git-commit-mode t)
(setq git-commit-summary-max-length 80)
(setq magit-save-some-buffers nil)
(setq magit-remote-ref-format 'remote-slash-branch)
(setq magit-completing-read-function 'ivy-completing-read)
(setq magit-commit-signoff t)

(global-set-key (kbd "<f3>") 'magit-status)

(setq magit-repository-directories
      '(("~/DEV/work/git/tripleo/UPSTREAM" . 2)
        ("~/DEV/work/git/tripleo/OOOQ/" . 2)
        ("~/DEV/work/git/laptop_config/" . 2)
        ("~/DEV/work/git/ansible/" . 2)))

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

(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

(setq projectile-completion-system 'ivy)
(setq python-shell-interpreter "/usr/bin/python3.7")
(setq python-shell-interpreter-args "")

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

;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(setq org-journal-file-type "yearly")
(setq org-journal-file-format "%Y")

(display-battery-mode 1)
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
(beacon-mode 1)

(setq org-hide-emphasis-markers t)
(add-to-list 'org-modules 'org-habit)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple (cond ((x-list-fonts "Agave")            '(:font "Agave"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          '(org-ellipsis ((t (:foreground "yellow1" :underline nil))))
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
		    "IDEA(i!)"
		    "GOAL(g!)"
                    "HABIT(h!)"
                    "DOING(s!)"
                    "BLOCKED(b@)"
                    "REVIEW(r!/!)"
                    "|"
                    "DONE(d!)"
                    "ARCHIVED(a@)"
                    "CANCELED(c@)")
	  )))

(with-eval-after-load 'org
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "LawnGreen" :weight bold :bold t))
          ("DOING" . (:foreground "DarkOrange" :weight bold :bold t))
          ("BLOCKED" . (:foreground "firebrick3" :weight bold :bold t))
          ("REVIEW" . (:foreground "cyan2" :weight bold :bold t))
          ("DONE" . (:foreground "red1"
                     :weight bold
                     :bold t
                     :underline t))
          ("CANCELED" . (:foreground "red3"
                         :weight bold
                         :bold t
                         :strike-through t))
          ("ARCHIVED" . (:foreground "SlateBlue"
                         :weight bold
                         :bold t
                         :strike-through t))
          ("HABIT" . (:foreground "#D6DA25"
                      :bold t
                      :weight bold
                      :box (:line-width 1 :style none)))
          ("GOAL" . (:foreground "#DDDD00"
                     :bold t
                     :weight bold
                     :box (:line-width 1 :style none)))
	  ("IDEA" . (:foreground "#F8B63F"
                     :bold t
                     :weight bold
                     :box (:line-width 1 :style none))))))

(load "~/.doom.d/mutt.el")
(load "~/.doom.d/muttrc-mode.el")
(add-to-list 'auto-mode-alist '(".*neomutt.*" . mutt-mode))
;; (add-hook 'mutt-mode-hook 'smyx)
;; wrap email body
(add-hook 'mutt-mode-hook 'turn-on-auto-fill)
(add-hook 'mutt-mode-hook 'turn-on-filladapt-mode)
(add-hook 'mutt-mode-hook 'flyspell-mode)

(setq mail-header-separator "")
