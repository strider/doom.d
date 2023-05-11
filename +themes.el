;;; +themes.el -*- lexical-binding: t; -*-

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:()

(require 'modus-themes)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)
(setq modus-themes-mixed-fonts t)
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-completions
      '((matches extrabold background intense)
        (selection semibold accented intense)
        (popup accented)))
(setq modus-themes-prompts '(intense background italic bold))
(setq modus-themes-headings
      '((1 . (background overline variable-pitch 1.5))
        (2 . (background overline rainbow 1.3))
        (3 . (overline 1.1))
        (t . (monochrome))))
(setq modus-themes-scale-headings t)

;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-intense)

;; Load the theme of your choice:
(load-theme 'modus-vivendi-tinted :no-confim)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
