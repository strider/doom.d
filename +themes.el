;;; +themes.el -*- lexical-binding: t; -*-

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:()

(require 'modus-themes)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-region '(bg-only no-extend))
(setq modus-themes-diffs 'desaturated)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-mode-line '(borderless))
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
