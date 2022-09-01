;;; early-init.el --- Pre-start initialization -*- lexical-binding: t -*-

;;; Commentary:

;;

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

;; Disable default package.el initialization
;;
;; init.el will call 'package-initialize _after_ properly configuring
;; package.el.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
