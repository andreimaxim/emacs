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

;; Probably the best git porcelain out there
(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook  ((prog-mode org-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package devdocs
  :ensure t
  :config
  (global-set-key (kbd "C-h D") 'devdocs-lookup)
  (setq devdocs-current-docs '("ruby~3.1" "rails~7.0")))

(use-package yard-mode
  :ensure t
  :diminish
  :hook (ruby-mode . yard-mode))

(use-package web-mode
  :ensure t
  :mode
  (".liquid")
  :custom
  (web-mode-enable-front-matter-block t))

;; Connect an Emacs REPL buffer to a Ruby subprocess.
(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

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

(provide 'andreimaxim-prog)
