;;; $$DOOMDIR/config.el -*- lexical-binding: t; -*-

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
;; '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 128 :width normal)))))
(setq doom-font (font-spec :family "JetBrains Mono" :size 14.0 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14.0 :weight 'normal)
      doom-big-font(font-spec :family "JetBrains Mono" :size 25))

;; Using Red Hat First logo as a banner! ;-)
(setq fancy-splash-image "~/Pictures/redhat-banner.png")
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
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; Use ido and flx-ido instead of ivy
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; LSP configuration
(setq lsp-enable-file-watchers nil)

 (add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/.local/bin")

(after! ivy
        ;; I prefer search matching to be ordered; it's more precise
        (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

(set-company-backend! 'org-mode '(company-yasnippet company-capf company-files company-elisp))
(setq company-idle-delay 0.25
      company-minimum-prefix-length 2)
(add-to-list 'company-backends '(company-capf company-files company-yasnippet))

(rainbow-mode 1)

(after! centaur-tabs
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'over
        centaur-tabs-modified-marker "⚠"
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-separator "/")

  (map! :leader
        :desc "Toggle tabs on/off"
        "t c" #'centaur-tabs-local-mode)
  (evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
    (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
    (kbd "g <down>")  'centaur-tabs-forward-group
    (kbd "g <up>")    'centaur-tabs-backward-group))

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
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "feh")
                              ("jpg" . "feh")
                              ("png" . "feh")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(if (display-graphic-p)
    (setq doom-theme 'modus-vivendi)
  (setq doom-theme 'smyx))

(setq modus-themes-no-mixed-fonts t)
(setq modus-themes-mode-line '3d)
(setq modus-themes-org-blocks 'grayscale)
(setq modus-themes-org-habit 'traffic-light)
(setq modus-themes-scale-headings t)
(setq modus-themes-syntax 'yellow-comments-green-strings)
(setq modus-themes-paren-match 'intense-bold)
(setq modus-themes-scale-1 1.0
      modus-themes-scale-2 1.1
      modus-themes-scale-3 1.25
      modus-themes-scale-4 1.5
      modus-themes-scale-5 1.75)

(global-anzu-mode +1)
(global-set-key (kbd "C-;") 'iedit-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-recursive-deletes 'always)
(setq dired-listing-switches "-alh")

(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.doom.d/snippets/"))

;; Disable fringe mode
(fringe-mode 0)
;; `org-indent-mode' does not play nice with git-gutter, so let's disable it
(setq git-gutter:disabled-modes '(org-mode))
(global-git-gutter+-mode 1)
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

(cua-mode t)
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

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
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
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

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

;; Enable projectile caching because doom doesn't enable by default
(setq projectile-enable-caching t)
(setq projectile-project-search-path "~/DEV/work/git/")
(setq projectile-completion-system 'ivy)
(setq python-shell-interpreter "/usr/bin/python3.7")
(setq python-shell-interpreter-args "")

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(define-key evil-normal-state-map (kbd "Q") (kbd "gqip"))
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

;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
(setq org-journal-file-type "yearly")
(setq org-journal-file-format "%Y")
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
(beacon-mode 1)

;; ;; Window numbering should start from "1" for each new frame.
(setq-default winum-scope 'frame-local)

(setq calendar-week-start-day 1) ; Weeks start on monday
(setq calendar-date-style 'european)
(setq european-calendar-style t)

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
        ("JIRA"        . "https://projects.engineering.redhat.com/browse/")
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

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! org (setq org-log-into-drawer "LOGBOOK"
                  org-log-done 'time
                  org-log-repeat 'time
                  org-log-redeadline 'note
                  org-log-reschedule 'note))

(after! org (setq org-use-property-inheritance t))

(setq org-ellipsis " ↴ ")

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

;; Loading mutt and muttrc mode because I am still using neomutt for my mails
(add-to-list 'auto-mode-alist '(".*neomutt.*" . message-mode))
;; wrap email body
(add-hook 'mutt-mode-hook 'turn-on-auto-fill)
(add-hook 'mutt-mode-hook 'turn-on-filladapt-mode)
(add-hook 'mutt-mode-hook 'flyspell-mode)

(load "~/.doom.d/muttrc-mode.el")
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
              auto-mode-alist))

;; (setq mail-header-separator "")

;; Shortcut for changing font-size
(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(define-key global-map (kbd "C-1") 'zoom-in)
(define-key global-map (kbd "C-0") 'zoom-out)

;; Elfeed configuration
(elfeed-org)
(after! elfeed
  (setq elfeed-search-filter "@4-month-ago +unread")
  (setq rmh-elfeed-org-files (list "~/.doom.d/private/elfeed.org"))
  (setq elfeed-db-directory "~/.elfeed/"))

(global-set-key (kbd "<f4>") 'elfeed)
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

;; Elfeed mapping
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'elfeed-search-quit-window
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      ;; :nm "q" #'+rss/delete-pane
      :nm "q" #'elfeed-search-quit-window
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)

  (setq elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'elfeed-search-quit-window
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (setq-local truncate-lines t)
      (setq-local shr-width 120)
      (setq-local line-spacing 0.2)
      (setq-local visual-fill-column-center-text t)
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))
  )

;; calibredb configuration
(after! calibredb
  (setq calibredb-root-dir "/run/media/gchamoul/SDCARD32/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))

;; Nov -- reading epub in emacs
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

;; (mu4e-alert-set-default-style 'libnotify)
;; (mu4e-alert-enable-notifications)
;; (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/emacs.svg")

;; (setq mu4e-alert-interesting-mail-query
;;       (concat
;;        "flag:unread"
;;        " AND NOT flag:trashed"
;;        " AND maildir:/Inbox"))

;; (setq mu4e-headers-fields
;;       '((:human-date    .  12)
;;         (:flags         .   6)
;;         (:from          .  40)
;;         (:subject       . nil)))

;; (setq  mu4e-maildir-shortcuts
;;        '(("/INBOX"            . ?i)
;;          ("/ML.announce-list" . ?a)
;;          ("/ML.cdg-list"      . ?c)
;;          ("/ML.France-list"   . ?f)
;;          ("/Perso"            . ?p)
;;          ("/ML.memo-list"     . ?m)
;;          ("/ML.OST-UPSTREAM-TRIPLEO-DEV"     . ?t)))

;; ;; Bookmarks
;; (setq mu4e-bookmarks
;;       `(("maildir:/INBOX" "Inbox" ?i)
;;         ("list:openstack-discuss.lists.openstack.org and subject:tripleo" "Tripleo DEV" ?o)
;;         ("list:openstack-discuss.lists.openstack.org and not subject:tripleo" "OpenStack DEV" ?u)
;;         ("maildir:/ML.Bugzilla" "Bugzilla"      ?b)
;;         ("maildir:/ML.Code-Reviews" "Code Reviews"      ?x)
;;         ("maildir:/ML.OST-RH-OPENSTACK-DEVEL" "RHOS-dev"       ?v)
;;         ("maildir:/ML.OST-RH-OPENSTACK-PGM" "RHOS-pgm"         ?g)
;;         ("maildir:/ML.OST-RHOS-TECH" "RHOS-tech"         ?c)
;;         ("maildir:/ML.OST-RH-DFG-DF" "DGF:DF"         ?q)
;;         ("maildir:/ML.memo-list" "memo-list"         ?m)
;;         ("maildir:/ML.Neurodiversity" "Neurodiversity list"         ?k)
;;         ("maildir:/ML.RH-Tech-list" "tech-list"         ?h)
;;         ("maildir:/ML.Remotees-list" "remotees-list"         ?e)
;;         ("maildir:/ML.announce-list" "announce-list"         ?j)
;;         ("maildir:/ML.GITHUB_WATCHING" "GitHub Watching"         ?d)
;;         ("maildir:/ML.Friday-list" "friday-list"         ?f)
;;         ("maildir:/ML.cdg-list" "cdg-list"         ?y)
;;         ("maildir:/ML.france-list" "france-list"         ?l)
;;         ("maildir:/ML.french-associates" "french-associates"         ?n)
;;         ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
;;         ("date:today..now" "Today's messages" ?t)
;;         ("date:7d..now" "Last 7 days" ?w)
;;         ("mime:image/*" "Messages with images" ?p)
;;         (,(mapconcat 'identity
;;                      (mapcar
;;                       (lambda (maildir)
;;                         (concat "maildir:" (car maildir)))
;;                       mu4e-maildir-shortcuts) " OR ")
;;          "All inboxes" ?a)))

;; (after! mu4e
;;   ;; use mu4e for e-mail in emacs
;;   (setq mu4e-root-maildir (expand-file-name "~/Mail/redhat-gmail"))
;;   (setq mu4e-compose-reply-to-address "gchamoul@redhat.com")
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   (setq mu4e-trash-folder "/Trash")
;;   (setq mu4e-refile-folder "/Archive")
;;   (setq mu4e-sent-folder "/Sent")
;;   (setq mu4e-drafts-folder "/Drafts")
;;   (setq mu4e-compose-signature-auto-include nil)
;;   (setq mu4e-view-show-images t)
;;   (setq mu4e-view-image-max-width 800)
;;   (setq mu4e-view-use-gnus nil)
;;   (setq mu4e-view-show-addresses t)
;;   (setq sendmail-program "/usr/bin/msmtp"
;;         send-mail-function #'smtpmail-send-it
;;         message-sendmail-f-is-evil t
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-auto-save-directory "~/Mail/redhat-gmail/Drafts"
;;         message-send-mail-function #'message-send-mail-with-sendmail
;;         mu4e-confirm-quit nil
;;         mu4e-view-fields '(:from :to :cc :bcc :subject :flags :date :maildir :mailing-list :tags :attachments :signature :decryption)
;;         mu4e-compose-format-flowed nil
;;         ;; fill-flowed-encode-column 80
;;         mu4e-headers-date-format "%+4Y-%m-%d"
;;         ;; mm-fill-flowed t
;;         message-kill-buffer-on-exit t
;;         mu4e-context-policy 'pick-first
;;         mu4e-confirm-quit nil
;;         mu4e-sent-messages-behavior 'sent
;;         mu4e-change-filenames-when-moving t
;;         mu4e-use-fancy-chars nil
;;         mu4e-split-view 'horizontal
;;         mu4e-index-cleanup nil      ;; don't do a full cleanup check
;;         mu4e-index-lazy-check t    ;; don't consider up-to-date dirs
;;         mu4e-headers-auto-update t
;;         mu4e-headers-include-related t
;;         mu4e-compose-dont-reply-to-self t
;;         mu4e-attachment-dir "~/Downloads"
;;         mu4e-view-prefer-html nil
;;         mu4e-html2text-command "w3m -dump -T text/html"
;;         mu4e-completing-read-function 'ivy-completing-read
;;         ;; every new email composition gets its own frame!
;;         mu4e-compose-in-new-frame t
;;         mu4e-update-interval (* 2 60)
;;         mu4e-index-update-in-background t
;;         message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
;;         ;; choose to use the formatted string
;;         message-citation-line-function 'message-insert-formatted-citation-line
;;         )

;;   (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
;;   (add-hook 'mu4e-compose-mode-hook 'orgtbl-mode)
;;   (add-hook 'mu4e-view-mode-hook #'visual-line-mode)
;;   (global-set-key (kbd "<f5>") 'mu4e-update-index)

;;   ;; this setting allows to re-sync and re-index mail
;;   ;; by pressing U
;;   (setq mu4e-get-mail-command  "mbsync -a")

;;   ;; Confirmation before sending
;;   (add-hook 'message-send-hook
;;             (lambda ()
;;               (unless (yes-or-no-p "Sure you want to send this?")
;;                 (signal 'quit nil))))

;;   (add-hook 'mu4e-compose-mode-hook
;;             (defun my-add-bcc ()
;;               "Add a Bcc: header."
;;               (save-excursion (message-add-header "Bcc: gchamoul@redhat.com\n"))));
;;   )

;; (setq mu4e-headers-thread-child-prefix '("├>" . "├▶ "))
;; (setq mu4e-headers-thread-last-child-prefix '("└>" . "└▶ "))
;; (setq mu4e-headers-thread-connection-prefix '("│" . "│ "))
;; (setq mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ "))
;; (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶ "))

;; Use imagemagick, if available.
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
;; (after! org-msg
;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;;         org-msg-startup "hidestars indent inlineimages"
;;         org-msg-greeting-fmt "\nHi *%s*,\n\n"
;;         org-msg-greeting-name-limit 3
;;         org-msg-default-alternatives '(text html)
;;         org-msg-convert-citation t
;;         org-msg-signature "

;;         Regards,

;;         #+begin_signature
;;         -- *Jeremy* \\\\
;;         /One Emacs to rule them all/
;;         #+end_signature")
;;   (org-msg-mode))

(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "gchamoul"
                     :sasl-username "gchamoul"
                     :sasl-password "lazypw"
                     :channels ("#emacs"))))
