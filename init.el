;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-
;;
;; Author:           Andrei Maxim
;; URL:              https://github.com/andreimaxim/dotfiles/emacs
;; Version:          0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv2
;;

;;; Commentary:


;;; Code:

;;; Package setup
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;;; Custom settings
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'andreimaxim-defaults)
(require 'andreimaxim-ui)
(require 'andreimaxim-ux)
(require 'andreimaxim-prog)
(require 'andreimaxim-org)


;; Reset the GC to a normal value after startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 50 1024 1024)
		  gc-cons-percentage 0.1)))
;;; init.el ends here
