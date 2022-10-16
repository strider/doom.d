;;; +internet.el -*- lexical-binding: t; -*-

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
