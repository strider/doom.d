;;; +git.el -*- lexical-binding: t; -*-

(global-git-commit-mode t)
(setq git-commit-style-convention-checks
      '(non-empty-second-line
        overlong-summary-line))

(after! magit
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; (setq ghub-use-workaround-for-emacs-bug 'force)
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

;; Allow forge to create repositories under my name
(setq forge-owned-accounts '(("strider")))

(add-hook! 'code-review-mode-hook #'emojify-mode)
(setq code-review-fill-column 80)
(setq code-review-new-buffer-window-strategy #'switch-to-buffer)
(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))
