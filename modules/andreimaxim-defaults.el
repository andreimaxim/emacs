;;; andreimaxim-defaults.el -*- lexical-binding: t; -*-

;; Keep customizations in a separate file that can be easily gitignored.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; Always confirm before exiting Emacs
(setq-default confirm-kill-emacs 'yes-or-no-p)

;;; Autosave and backup

;; Store autosave files and backups in /tmp so they don't pollute the filesystem
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Autosaving can be done extremely frequently due to modern SSDs
(setq auto-save-interval 5)


;;; Buffers

;; Generate unique buffer names
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

;; Autorefresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;; Package
(provide 'andreimaxim-defaults)
