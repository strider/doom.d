;;; +text.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/CloudStorage/Dropbox/org/")
(setq org-noter-notes-search-path '("~/Library/CloudStorage/Dropbox/org/noter/"))

;; Disable line-numbers for org and text modes
(add-hook 'org-mode-hook #'doom-disable-line-numbers-h)
(add-hook 'text-mode-hook #'doom-disable-line-numbers-h)

(setq org-journal-file-type 'yearly)
(setq org-journal-file-format "%Y")
(setq org-journal-date-format "%A, %m/%d/%Y")
(setq org-journal-enable-agenda-integration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs text rendering optimizations
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

;; Only render text left to right
(setq-default bidi-paragraph-direction 'left-to-right)

(after! org-roam
  (setq org-roam-directory "~/Library/CloudStorage/Dropbox/org/roam"))

(after! org-roam
  (setq org-roam-capture-templates
        `(("d" "default" plain
           (file ,(expand-file-name "templates/roam-default.org" doom-private-dir))
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t))))

(org-roam-db-autosync-mode)

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
                   (org-agenda-files '("~/Library/CloudStorage/Dropbox/org/work.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                               'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Personal")
                   (org-agenda-files '("~/Library/CloudStorage/Dropbox/org/personal.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                               'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Ideas & Goals")
                   (org-agenda-files '("~/Library/CloudStorage/Dropbox/org/ideas-goals.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                               'scheduled))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Personal Habits")
                   (org-agenda-files '("~/Library/CloudStorage/Dropbox/org/habits.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header " Projects")
                   (org-agenda-files '("~/Library/CloudStorage/Dropbox/org/projects.org"))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
                                               'scheduled))))
            (todo "TODO"
                  ((org-agenda-files '("~/Library/CloudStorage/Dropbox/org/reading.org"))
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

;; (with-eval-after-load 'org
;;   (setq org-capture-templates
;;         '(("x" "TODO PERSONAL Tasks" entry (file+headline "~/Library/CloudStorage/Dropbox/org/personal.org" "PERSONAL TASKS")
;;            "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
;;            :prepend t :kill-buffer t)
;;           ("p" "Templates for projects")
;;           ("pt" "Project TODO" entry (file+headline "~/Library/CloudStorage/Dropbox/org/projects.org" 'projectile-project-name)
;;            "* TODO %?\n%i\n%a" :prepend t)
;;           ("w" "TODO WORK Tasks" entry (file+headline "~/Library/CloudStorage/Dropbox/org/work.org" "WORK TASKS")
;;            "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
;;            :prepend t :kill-buffer t)
;;           ("i" "IDEA or GOAL" entry (file+headline "~/Library/CloudStorage/Dropbox/org/ideas-goals.org" "IDEAS AND GOALS")
;;            "\n* TODO %\\1 - %\\2%?\n:PROPERTIES:\n:DESCRIPTION: %^{DESCRIPTION}\n:TITLE: %^{TITLE}\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n"
;;            :prepend t :kill-buffer t)
;;           ("h" "HABIT" entry (file+headline "~/Library/CloudStorage/Dropbox/org/habits.org" "HABITS")
;;            "\n* TODO %\\1%?\n:PROPERTIES:\n:TITLE: %^{TITLE}\n:STYLE: habit\n:END:\n:LOGBOOK:\n- Added: %u\n:END:\n" :prepend t :kill-buffer t)
;;           ("c" "Contacts" entry (file "~/Library/CloudStorage/Dropbox/org/contacts.org")
;;            "* %\\1%?\n:PROPERTIES:\n:NAME: %^{NAME}\n:EMAIL:\n:PHONE:\n:ALIAS:\n:NICKNAME:\n:NOTE:\n:ADDRESS:\n:BIRTHDAY:\n:END:" :prepend t :kill-buffer t)
;;           )))

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

(with-eval-after-load "org-tree-slide"
  (defvar my-hide-org-meta-line-p nil)
  (defun my-hide-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p t)
    (set-face-attribute 'org-meta-line nil
                        :foreground (face-attribute 'default :background)))
  (defun my-show-org-meta-line ()
    (interactive)
    (setq my-hide-org-meta-line-p nil)
    (set-face-attribute 'org-meta-line nil :foreground nil))

  (defun my-toggle-org-meta-line ()
    (interactive)
    (if my-hide-org-meta-line-p
        (my-show-org-meta-line) (my-hide-org-meta-line)))

  (add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)
  (add-hook 'org-tree-slide-stop-hook #'my-show-org-meta-line))

(when (require 'org-tree-slide nil t)
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

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(defun gc/my-split-and-indirect-orgtree ()
  "Splits window to the right and opens an org tree section in it"
  (interactive)
  (split-window-right)
  (windmove-right)
  (org-tree-to-indirect-buffer))

(defun gc/my-kill-and-unsplit-orgtree ()
  "Kills the cloned buffer and deletes the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

(evil-define-command +evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)
        (setq-local doom-real-buffer-p t)))))

(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+end_src" . "«")
                           ("#+END_SRC" . "«")
                           ("#+begin_src" . "»")
                           ("#+BEGIN_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

(setq org-startup-folded 'content)

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))

(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(use-package! doct
  :defer t
  :commands (doct))

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ((equal pressed "ESC") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
        (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                               (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                    "\t"
                                                                    (nth 1 template))))
                                   template)
                                 templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

(after! org-capture
  (setq org-capture-templates
        (doct `(("Home" :keys "h"
                 :icon ("home" :set "octicon" :color "cyan")
                 :file "~/Library/CloudStorage/Dropbox/org/Home.org"
                 :prepend t
                 :headline "HOME TASKS"
                 :template ("* TODO %?"
                            "%i %a"))
                ("Personal" :keys "p"
                 :icon ("home" :set "octicon" :color "cyan")
                 :file "~/Library/CloudStorage/Dropbox/org/personal.org"
                 :prepend t
                 :headline "PERSONAL TASKS"
                 :template ("* TODO %?"
                            "%i %a"))
                ("Ideas and Goals" :keys "i"
                 :icon ("home" :set "octicon" :color "cyan")
                 :file "~/Library/CloudStorage/Dropbox/org/ideas-goals.org"
                 :prepend t
                 :headline "IDEAS AND GOALS"
                 :template ("* TODO %r"
                            "%i %a"))
                ("Work" :keys "w"
                 :icon ("business" :set "material" :color "yellow")
                 :file "~/Library/CloudStorage/Dropbox/org/work.org"
                 :prepend t
                 :headline "WORK TASKS"
                 :template ("* TODO %?"
                            "SCHEDULED: %^{Schedule:}t"
                            "DEADLINE: %^{Deadline:}t"
                            "%i %a"))
                ("Note" :keys "n"
                 :icon ("sticky-note" :set "faicon" :color "yellow")
                 :file "~/Library/CloudStorage/Dropbox/org/notes.org"
                 :template ("* *?"
                            "%i %a"))
                ("Journal" :keys "j"
                 :icon ("calendar" :set "faicon" :color "pink")
                 :type plain
                 :function (lambda ()
                             (org-journal-new-entry t)
                             (unless (eq org-journal-file-type 'daily)
                               (org-narrow-to-subtree))
                             (goto-char (point-max)))
                 :template "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                 :jump-to-captured t
                 :immediate-finish t)
                ("Project" :keys "p"
                 :icon ("repo" :set "octicon" :color "silver")
                 :prepend t
                 :type entry
                 :headline "Inbox"
                 :template ("* %{keyword} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:keyword "")
                 :children (("Task" :keys "t"
                             :icon ("checklist" :set "octicon" :color "green")
                             :keyword "TODO"
                             :file +org-capture-project-todo-file)
                            ("Note" :keys "n"
                             :icon ("sticky-note" :set "faicon" :color "yellow")
                             :keyword "%U"
                             :file +org-capture-project-notes-file)))
                ))))
