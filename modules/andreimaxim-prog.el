;;; andreimaxim-prog.el ---  Programming setup -*- lexical-binding: t; -*-

;;; Commentary:

;; 

;;; Code:

;; Parse-tree-based syntax highlighting.
;;
;; Unfortunately, tree-sitter only supports a subset of languages, as they
;; are defined in the `tree-sitter-langs' package.
(use-package tree-sitter
  :ensure t
  :hook ((css-mode go-mode html-mode javascript-mode json-mode typescrypt-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

;; Better shell within Emacs.
;; Requires libtool-bin system package.
(use-package vterm
  :ensure t)

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

(use-package magit
  :ensure t)

(use-package eglot
  :ensure t
  :hook (ruby-mode . eglot-ensure))

(use-package consult-eglot
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

;; Connect an Emacs REPL buffer to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))


(use-package uuidgen
  :ensure t)

(provide 'andreimaxim-prog)
;;; andreimaxim-prog.el ends here
