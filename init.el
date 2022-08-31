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

;; Byte-compiled Elisp-modules should be automatically queued for native
;; compilation.
(setq native-comp-deferred-compilation t)

;;; Better defaults

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab space equivalence
(setq-default tab-width 2)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; By default, Emacs centers the point vertically whenever the point goes
;; off the screen, which is different to what everything else does (see
;; `Automatic Scrolling' in the manual).
;;
;; Setting `scroll-conservatively' to a value over 100, automatic scrolling
;; does not center the point.
(setq scroll-conservatively 101)

;; Use the visual bell instead of a sound
(setq-default visible-bell t)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

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

;; Do not create any backups as any text that's worth a backup will be stored
;; in git or something similar.
(setq make-backup-files nil)

;; Autorefresh buffers
(global-auto-revert-mode 1)

;; Remember recently edited files
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

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
(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

;; Lower the contrast for line numbers
(set-face-foreground 'line-number "gray90")

;;; Fonts
;;
;; The PragmataPro font with adition OTF files should be already installed.
;;
;; An free alternative is Iosevka Term SS08 for default and fixed-pitch faces,
;; and Iosevka Aile for non-monospace text.
(set-face-attribute 'default nil :family "PragmataPro Mono Liga" :height 120)
(set-face-attribute 'fixed-pitch nil :font "PragmataPro Mono Liga")
(set-face-attribute 'variable-pitch nil :family "PragmataPro Mono Liga")

;;; The ligature support is not distributed as a package on ELPA or MELPA
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package pragmatapro-lig
  :ensure nil
  :hook (prog-mode . pragmatapro-lig-mode))

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

;; Does not work with Emacs older than 29.1
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :custom (show-paren-context-when-offscreen 'overlay))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project))

;; Remove modes from modeline
(use-package diminish
  :ensure t
  :init
  (diminish 'rainbow-mode)
  (diminish 'eldoc-mode))

;;; UX improvements

;; These are a set of Emacs packages that enhance the user experience.
;;
;; The general approach here is to either configure built-in packages or use
;; packages that rely on the default Emacs functionality instead of reinventing
;; the wheel.

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

;; Whenever you enter an incomplete command (say `C-x p'), which-key will show
;; in the minibuffer the available key bindings that follow `C-x p'.
(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Add information about buffers, lines and so on.
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 1)
  (corfu-auto-prefix 3)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

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

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


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
  :diminish t
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

(use-package yasnippet
  :ensure t
  :diminish t
  :hook  ((prog-mode org-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :diminish t)

(use-package smartparens
  :ensure t)

(use-package enh-ruby-mode
  :ensure t)

(use-package devdocs
  :ensure t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup)
  (setq devdocs-current-docs '("ruby~3.1" "rails~7.0")))

(use-package yard-mode
  :ensure t
  :diminish
  :hook (enh-ruby-mode . yard-mode))

(use-package web-mode
  :ensure t
  :mode
  (".liquid")
  :custom
  (web-mode-enable-front-matter-block t))

;; Connect an Emacs REPL buffer to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package markdown-mode
  :ensure t)

;; For UUIDv4 generation
(use-package uuidgen
  :ensure t)

;; Recommended settings from org-modern-mode
(use-package org
  :ensure t
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))

;; Better UI for Org
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" default))
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow monokai-pro-theme blackboard-theme flymake-aspell all-the-icons doom-modeline leuven-theme ligature ligatures ef-themes projectile-rails smartparens-ruby smartparens enh-ruby-mode faff-theme zenburn-theme kaolin-themes sqlformat docker yasnippet-snippets yard-mode yaml-mode ws-butler vterm vertico uuidgen orderless mood-line markdown-mode marginalia kind-icon json-mode diminish corfu bundler ace-window))
 '(safe-local-variable-values
   '((setq-local devdocs-current-docs quote
                 ("ruby~3.1" "rails~7.0"))
     (eval setq-local devdocs-current-docs
           '("ruby~3.1" "rails~7.0"))
     (eval setq-local devdocs-current-docs
           '("ruby~3.1, rails~7.0"))
     (eval setq-local devdocs-current-docs
           '("ruby~3.1")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
