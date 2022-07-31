;;; init.el --- Emacs  -*- lexical-binding: t -*-
;;
;; Author:           Andrei Maxim
;; URL:              https://github.com/andreimaxim/emacs.d
;; Version:          0.9.0
;;
;; This file is not part of GNU Emacs.
;;
;; License: MIT
;;

;;; Commentary:


;;; Code:

;;; Package setup
;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;;; Better defaults

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab space equivalence
(setq-default tab-width 4)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Use the visual bell instead of a sound
(setq-default visible-bell t)

;; Always show information regarding the current column
(setq column-number-mode t)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Text mode is initial mode
(setq initial-major-mode 'org-mode)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(menu-bar-mode -1)


;; Navigate windows using shift+direction
(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)

;; Use the new scroll mode which should be smoother on Emacs 29+
(pixel-scroll-precision-mode t)

;; Autorefresh buffers
(global-auto-revert-mode 1)

;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;;; UI tweaks
;; The tsdh-light theme is built into Emacs and it's slightly better
;; than the default theme.
(load-theme 'tsdh-light)

;; Add a bit of padding on all sides
(modify-all-frames-parameters
 '((internal-border-width . 20)))

;; A lot of themes will show vertical bars of a slightly different color due
;; to the extra padding added above.
(dolist (face '(window-divider
		        window-divider-first-pixel
		        window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;;; Fonts
;;
;; The block below assumes that the following two Iosevka fonts are instaled:
;; * Iosevka Term ss08 (the PragmataPro variant)
;; * Iosevka Aile
;;
;; Iosevka Term is used because it's slightly narrower than the default Iosevka,
;; while the Aile is used for non-monospace text.
(set-face-attribute 'default nil :family "Iosevka Term SS08" :height 135)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Term SS08")
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

;; Smaller text size alternative
;; (set-face-attribute 'default nil :family "PragmataPro Mono Liga" :height 120)
;; (set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono Liga" :height 120)
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

;; Generate unique buffer names
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package savehist
  :ensure nil
  :init (savehist-mode))


;; Cleaner modeline
(use-package mood-line
  :ensure t
  :init (mood-line-mode))

;; Remove modes from modeline
(use-package diminish
  :ensure t
  :init
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; UX improvements

;; These are a set of Emacs packages that enhance the user experience.
;;
;; The general approach here is to either configure built-in packages or use
;; packages that rely on the default Emacs functionality instead of reinventing
;; the wheel.

;; Whenever you enter an incomplete command (say `C-x p'), which-key will show
;; in the minibuffer the available key bindings that follow `C-x p'.
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Easily switch between multiple windows using M-o
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

;; A much more polished dired experience
(use-package dirvish
  :ensure t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  :bind
  ("C-x d" . dirvish))

(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Add information about buffers, lines and so on.
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless flex)
	    completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package corfu
  :ensure t
  ;; :custom
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; Consult provides complex actions based on whatever was typed:
;; jump to buffer, open a new window, open a buffer in the project, etc.
;;
;; The keybindings are the one suggested by the package author.
(use-package consult
  :ensure t
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	     ("C-x p g" . consult-ripgrep)		   ;; orig project-find-regexp
	     ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)

         ("C-s" . consult-line)                    ;; Override Isearch
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;;; Programming

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; The default electric-indent will indent only on return, which feels a bit
;; too late.
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode t))

;; Automatically fix whitespaces, but only the ones we've changed.
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

;; Parse-tree-based syntax highlighting.
;;
;; Unfortunately, tree-sitter only supports a subset of languages, as they
;; are defined in the `tree-sitter-langs' package.
(use-package tree-sitter
  :ensure t
  :hook ((css-mode go-mode html-mode javascript-mode json-mode ruby-mode typescrypt-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

;; Probably the best git porcelain out there
(use-package magit
  :ensure t)


;; Flycheck seems to have a better UI than Flymake when it comes to reporting
;; errors.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; Disable ruby-rubylint as it's no longer maintained and reek because it
  ;; adds little value over Rubocop.
  (setq-default flycheck-disabled-checkers '(ruby-rubylint ruby-reek))
  (setq flycheck-emacs-lisp-load-path 'inherit))


;; Better shell within Emacs.
;; Requires libtool-bin system package.
(use-package vterm
  :ensure t
  :custom
  (vterm-always-compile-module t))


(use-package yasnippet
  :ensure t
  :hook  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package devdocs
  :ensure t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup))

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yard-mode
  :ensure t
  :diminish
  :hook (ruby-mode . yard-mode))

(use-package web-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

;; Provides `bundle-install' and `bundle-update' functions to install and
;; update bundles.
(use-package bundler
  :ensure t)

;; Connect an Emacs REPL buffer to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

;; For UUIDv4 generation
(use-package uuidgen
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dash-docs devdocs eglot which-key robe org yasnippet-snippets yard-mode yaml-mode ws-butler web-mode vterm vertico uuidgen use-package tree-sitter-langs rainbow-delimiters org-modern orderless mood-line marginalia magit lsp-mode kind-icon json-mode flycheck dirvish diminish corfu consult bundler all-the-icons aggressive-indent ace-window))
 '(safe-local-variable-values '((setq devdocs-current-docs '("ruby~3.1")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-icons
 ;; custom-set-icons was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


