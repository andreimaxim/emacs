;;; andreimaxim-ui.el -*- lexical-binding: t; -*-

;; Simplify the main Emacs frame by removing the menubar, toolbar and scrollbars.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Add a bit of padding on all sides 
(modify-all-frames-parameters
 '((internal-border-width . 20)))

;; Disable the initial startup screen
(setq-default inhibit-startup-screen t)

;; Use the visual bell instead of a sound
(setq-default visible-bell t)

;; Always show information regarding the current column
(setq column-number-mode t)

;; Always display the line numbers when editing source code
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;; Color delimiters based on their nested depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; The tsdh-light theme is built into Emacs and it's slightly better
;; than the default theme.
(load-theme 'tsdh-light)

;;; Fonts
;;
;; The block below assumes that the following two Iosevka fonts are instaled:
;; * Iosevka Term ss08 (the PragmataPro variant)
;; * Iosevka Aile
;;
;; Iosevka Term is used because it's slightly narrower than the default Iosevka,
;; while the Aile is used for non-monospace text.
(set-face-attribute 'default nil :family "Iosevka Term SS08" :height 135)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term SS08" :height 135)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 135)

;; A lot of themes will show vertical bars of a slightly different color due
;; to the extra padding added above.
(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; start the initial frame maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package mood-line
  :ensure t
  :init (mood-line-mode))

(use-package diminish
  :ensure t
  :init
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(provide 'andreimaxim-ui)
;;; andreimaxim-ui.el ends here
