;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

(package! org-bullets
  :recipe (:host github :repo "sabof/org-bullets"))

(package! keychain-environment
  :recipe (:host github :repo "tarsius/keychain-environment"))

(package! shanty-themes
  :recipe (:host github :repo "qhga/shanty-themes"))

(package! flycheck-vale
  :recipe (:host github :repo "abingham/flycheck-vale"))

(package! expand-region)

(package! typoel
  :recipe (:host github :repo "jorgenschaefer/typoel"))

(package! beacon
  :recipe (:host github :repo "Malabarba/beacon"))

(package! aggressive-indent)

(package! ivy-yasnippet
  :recipe (:host github :repo "mkcms/ivy-yasnippet"))

(package! smyx-theme)
(package! grandshell-theme)
(package! auto-yasnippet)
(package! filladapt)
(package! rpm-spec-mode
  :recipe (:host github :repo "stigbjorlykke/rpm-spec-mode"))

(package! iedit
  :recipe (:host github :repo "victorhge/iedit"))

;; (package! mu4e-views
;;   :recipe (:host github :repo "lordpretzel/mu4e-views"))

(package! peep-dired)
(package! dmenu)
;;(package! ivy-posframe)
(package! rainbow-mode)
(package! async)
(package! git-gutter+)
(package! lsp-pyright)
(package! elfeed)
(package! elfeed-org)
(package! jinja2-mode)
;; (package! flx-ido)
(package! lsp-ui)
(package! fzf)
(package! muttrc-mode
  :recipe (:host github :repo "neomutt/muttrc-mode-el"))

(package! pr-review
  :recipe (:host github :repo "blahgeek/emacs-pr-review"))

(package! vscode-dark-plus-emacs-theme
  :recipe (:host github :repo "ianyepan/vscode-dark-plus-emacs-theme"))

(package! ob-mermaid
  :recipe (:host github :repo "arnm/ob-mermaid"))

(package! mermaid-mode
  :recipe (:host github :repo "abrochard/mermaid-mode"))

(package! sunrise-commander
  :recipe (:host github :repo "sunrise-commander/sunrise-commander"))

(package! adoc-mode
  :recipe (:host github :repo "sensorflo/adoc-mode"))

(package! company :disable t)

;; (package! corfu
;;   :recipe (:host github :repo "minad/corfu"))

;; (package! corfu-doc
;;   :recipe (:host github :repo "galeo/corfu-doc"))

;; (package! kind-icon)
;; (package! orderless
;;   :recipe (:host github :repo "oantolin/orderless"))

;; (package! cape
;;   :recipe (:host github :repo "minad/cape"))

(package! focus
  :recipe (:host github :repo "larstvei/Focus"))

(package! poet
  :recipe (:host github :repo "kunalb/poet"))

(package! modus-theme
  :recipe (:host gitlab :repo "protesilaos/modus-themes"))

(package! emacs-doom-themes
  :recipe (:host github :repo "hlissner/emacs-doom-themes"))

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia"))

(package! mu4e-alert :disable t)
(package! alert
  :recipe (:host github :repo "jwiegley/alert"))

(package! osx-browse
  :recipe (:host github :repo "rolandwalker/osx-browse"))

(package! go-dlv
  :recipe (:host github :repo "benma/go-dlv.el"))

(package! vagrant
  :recipe (:host github :repo "ottbot/vagrant.el"))

(package! syntree
  :recipe (:host github :repo "enricoflor/syntree"))

(package! vagrant-tramp
  :recipe (:host github :repo "dougm/vagrant-tramp"))

(package! org-special-block-extras
  :recipe (:host github :repo "alhassy/org-special-block-extras"))

;; (package! nov)

;; (package! erc-hl-nicks
;;   :recipe (:host github :repo "leathekd/erc-hl-nicks"))
;; (package! org-msg :disable t)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
