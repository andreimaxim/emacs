;; Load the env variables, especially rbenv settings
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

(use-package magit
  :ensure t)

(use-package tree-sitter
  :ensure t
  :hook ((css-mode go-mode html-mode javascript-mode json-mode ruby-mode typescrypt-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
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
  :hook (ruby-mode . inf-ruby-minor-mode)
  :custom (inf-ruby-console-environment "development"))

(use-package seeing-is-believing
  :ensure t
  :hook (ruby-mode . seeing-is-believing))

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

(use-package align
	:ensure nil
	:config
	(add-to-list 'align-rules-list
							 '(ruby-comma-delimiter
								 (regexp . ",\\(\\s-*\\)[^# \t\n]")
								 (repeat . t)
								 (modes  . '(ruby-mode))))

	(add-to-list 'align-rules-list
							 '(ruby-hash-literal
								 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
								 (group 2 3)
								 (repeat . t)
								 (modes  . '(ruby-mode))))

	(add-to-list 'align-rules-list
							 '(ruby-hash-literal2
								 (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
								 (repeat . t)
								 (modes  . '(ruby-mode))))

	(add-to-list 'align-rules-list
							 '(ruby-assignment-literal
								 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
								 (repeat . t)
								 (modes  . '(ruby-mode))))

	(add-to-list 'align-rules-list
							 '(ruby-xmpfilter-mark
								 (regexp . "\\(\\s-*\\)# => [^#\t\n]")
								 (repeat . nil)
								 (modes  . '(ruby-mode)))))

(provide 'andreimaxim-prog)
