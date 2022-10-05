;;; ui/telephone-line/config.el -*- lexical-binding: t; -*-

(when (modulep! +keycast)
  (load! "+keycast"))

(use-package flycheck-indicator
  :after flycheck
  :config
  (setq flycheck-indicator-icon-error (string-to-char "!"))
  (setq flycheck-indicator-icon-info (string-to-char "·"))
  (setq flycheck-indicator-icon-warning (string-to-char "*"))
  (setq flycheck-indicator-status-icons
        '((not-checked "%")
          (no-checker "-")
          (running "&")
          (errored "!")
          (finished "=")
          (interrupted "#")
          (suspicious "?")))
  :hook (flycheck-mode-hook . flycheck-indicator-mode))

(defun mode-line-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset 0))
                      (propertize label 'face face))
              'help-echo help-echo))

(defvar selected-window (selected-window))
(defun active ()
  (eq (selected-window) selected-window))
(add-hook! 'pre-redisplay-functions
  (defun set-selected-window (&rest _)
    "Set the variable `selected-window' appropriately."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq selected-window (frame-selected-window))))))

(use-package! minions
  :when (modulep! +minions)
  :hook (after-init . minions-mode)
  :init
  (telephone-line-defsegment* telephone-line-minions-segment ()
    minions-mode-line-modes))

(use-package! anzu
  :after-call isearch-mode
  :config
  ;; anzu and evil-anzu expose current/total state that can be displayed in the
  ;; mode-line.
  (defun doom-modeline-fix-anzu-count (positions here)
    "Calulate anzu counts via POSITIONS and HERE."
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))

  (advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

  (setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state
                              anzu--cached-count anzu--cached-positions anzu--last-command
                              anzu--last-isearch-string anzu--overflow-p)))
(use-package! evil-anzu
  :when (modulep! :editor evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight))

(defun mode-line--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format "%d replace" anzu--cached-count))
             ((eq anzu--state 'replace)
              (format "%d/%d" here total))
             (anzu--overflow-p
              (format "%s+" total))
             (t
              (format "%s/%d" here total))))
     'face (if (active) 'mode-line-highlight))))

(defun mode-line--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format "%s matches" (how-many pattern (car range) (cdr range)))
         "-"))
     'face (if (active) 'mode-line-highlight))))

(defun mode-line--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p evil-mc-cursor-list)
    (let ((count (length evil-mc-cursor-list)))
      (when (> count 0)
        (let ((face (cond ((not (active)) 'mode-line-inactive)
                          (evil-mc-frozen 'mode-line-highlight)
                          ('mode-line-success-highlight))))
          (concat (propertize " " 'face face)
                  (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                  (propertize " " 'face `(:inherit (variable-pitch ,face)))
                  (propertize (format "%d " count)
                              'face face)))))))

(defun mode-line--overlay< (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defun mode-line--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion
                          (iedit-prev-occurrence)
                          (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format "%s/%d"
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'mode-line--overlay<)))
                      -1)
                 "-")
               length))
     'face (if (active) 'mode-line-highlight))))

(defun mode-line--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active)
             (or defining-kbd-macro
                 executing-kbd-macro))
    (let ((sep (propertize " " 'face 'mode-line-highlight)))
      (concat (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'mode-line-highlight)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'mode-line-highlight
                                     :v-adjust -0.05)
              sep))))

(defvar mode-line-matches
  '(:eval
    (let ((meta (concat (mode-line--macro-recording)
                        (mode-line--anzu)
                        (mode-line--evil-substitute)
                        (mode-line--iedit)
                        (mode-line--multiple-cursors))))
      meta)))
(put 'mode-line-matches 'risky-local-variable t)
(telephone-line-defsegment telephone-line-matches-segment ()
  mode-line-matches)


(setq-default
 mode-line-buffer-identification ; slightly more informative buffer id
 '((:eval
    (propertize
     (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
       (or (when buffer-file-name
             (if-let (project (doom-project-root buffer-file-name))
                 (let ((filename (or buffer-file-truename (file-truename buffer-file-name))))
                   (file-relative-name filename (concat project "..")))))
           "%b"))
     'face (cond ((buffer-modified-p)
                  '(error mode-line-buffer-id))
                 ((active)
                  'mode-line-buffer-id))
     'help-echo buffer-file-name))
   (buffer-read-only (:propertize " RO" face warning))))

(telephone-line-defsegment* telephone-line-doom-buffer-segment ()
  mode-line-buffer-identification)

(telephone-line-defsegment* telephone-line-gagbo-flymake-segment ()
  (when (bound-and-true-p flymake-mode)
    (telephone-line-raw flymake-mode-line-format t)))

(telephone-line-defsegment* telephone-line-doom-checker-segment ()
  (when (bound-and-true-p flycheck-mode)
    (pcase flycheck-last-status-change
      (`finished
       (if flycheck-current-errors
           (let-alist (flycheck-count-errors flycheck-current-errors)
             (let ((error (or .error 0))
                   (warning (or .warning 0))
                   (info (or .info 0)))
               (mode-line-format-icon "do_not_disturb_alt"
                                      (number-to-string (+ error warning info))
                                      (cond ((> error 0)   'error)
                                            ((> warning 0) 'warning)
                                            ('success))
                                      (format "Errors: %d, Warnings: %d, Debug: %d"
                                              error
                                              warning
                                              info))))
         (mode-line-format-icon "check" "" 'success)))
      (`running     (mode-line-format-icon "access_time" "*" 'font-lock-comment-face "Running..."))
      (`errored     (mode-line-format-icon "sim_card_alert" "!" 'error "Errored!"))
      (`interrupted (mode-line-format-icon "pause" "!" 'font-lock-comment-face "Interrupted"))
      (`suspicious  (mode-line-format-icon "priority_high" "!" 'error "Suspicious")))))

(defface telephone-line-gagbo-highlight-active-face
  `((t (:background ,(face-background 'lazy-highlight) :foreground ,(face-foreground 'default) :italic t :inherit mode-line)))
  "Face for the gagbo-highlight special"
  :group 'telephone-line)


(use-package! telephone-line
  :hook (after-init . telephone-line-mode)
  :init
  (require 'telephone-line-separators)
  (setq solaire-mode-remap-modeline nil)
  (setq telephone-line-faces
        '((gagbo-highlight . (telephone-line-gagbo-highlight-active-face . mode-line-inactive))
          (evil            . telephone-line-modal-face)
          (accent          . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil             . (mode-line . mode-line-inactive)))

        telephone-line-lhs
        '((evil            . (telephone-line-evil-tag-segment))
          (gagbo-highlight . (telephone-line-matches-segment))
          (accent          . (telephone-line-vc-segment
                              telephone-line-process-segment))
          (nil             . (telephone-line-doom-buffer-segment
                              telephone-line-airline-position-segment)))

        telephone-line-rhs
        '((nil    . (telephone-line-minions-segment
                     telephone-line-misc-info-segment))
          (accent . (;; telephone-line-doom-checker-segment
                     telephone-line-flycheck-segment
                     telephone-line-gagbo-flymake-segment)))

        telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right
        telephone-line-height 24
        telephone-line-evil-use-short-tag t))
