;; -*- no-byte-compile: t; -*-
;;; ui/telephone-line/packages.el

(when (modulep! +minions)
  (package! minions))
(when (modulep! +keycast)
  (package! keycast))
(package! anzu)
(package! evil-anzu)
(package! telephone-line)
(when (modulep! :checkers syntax)
  (package! flycheck-indicator))
