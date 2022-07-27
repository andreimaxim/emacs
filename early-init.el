;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Customizations that should be loaded early, before the package system and GUI
;; are initialized.

;;; Code:

;; Avoid garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)


;; Native compilation settings
(when (featurep 'native-compile)
  ;; Ignore compiler warnings
  (setq native-comp-async-report-warnings-errors nil)
  ;; Async compilation
  (setq native-comp-deferred-compilation t))
