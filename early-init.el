;;; early-init.el --- Pre-start initialization -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

;; Disable default package.el initialization
;;
;; init.el will call 'package-initialize _after_ properly configuring
;; package.el.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
