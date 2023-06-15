;;; enhanced-project-for-rails.el --- Enhanced Project for Rails
;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Andrei Maxim
;;
;; Author: Andrei Maxim
;; Maintainer: Andrei Maxim
;; Created: June 15, 2023
;; Modified: June 15, 2023
;; Version: 0.0.1
;; Keywords: project, rails
;; Homepage: https://github.com/andreimaxim/enhanced-project-for-rails
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package enhances project.el for Rails projects.
;;
;;; Code:

(require 'project)
(require 'inf-ruby)
(require 'ansi-color)

(defun enhanced-project-for-rails-root ()
  "Return the root of the current project or nil if not in a project."
  (when-let ((project (project-current)))
    (project-root project)))

(defun enhanced-project-for-rails-p ()
  "Return non-nil if the current project is a Rails project."
  (let ((root (enhanced-project-for-rails-root)))
    (and root
         (file-exists-p (expand-file-name "Gemfile" root))
         (file-exists-p (expand-file-name "config.ru" root)))))

;;;###autoload
(defun enhanced-project-for-rails-console ()
  "Start the Rails console in the current project directory."
  (interactive)
  (let ((default-directory (enhanced-project-for-rails-root))
        (inf-ruby-console-environment "development"))
    (inf-ruby-console-auto)))

(defun enhanced-project-for-rails--colorize-compilation-buffer ()
  "Colorize output in the compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;;;###autoload
(defun enhanced-project-for-rails-server ()
  "Start the Rails server in the current project directory."
  (interactive)
  (let* ((default-directory (enhanced-project-for-rails-root))
         (compilation-buffer-name-function (lambda (mode) "*Rails Server*")))
    (compile "bin/rails s")
    (with-current-buffer "*Rails Server*"
      (add-hook 'compilation-filter-hook 'enhanced-project-for-rails--colorize-compilation-buffer nil t))))

(provide 'enhanced-project-for-rails)
;;; enhanced-project-for-rails.el ends here
