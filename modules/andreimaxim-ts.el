;;;; andreimaxim-ts --- Tree Sitter integration -*- lexical-binding: t -*-

;;; Commentary:

;; Configures the Tree Sitter integration in Emacs based on the following
;; article:
;;
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

;;; Code:
(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Map major modes to their Tree Sitter variant
(setq major-mode-remap-alist
      '((css-mode . css-ts-mode)
        (js2-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(provide 'andreimaxim-ts)
;;; andreimaxim-ts.el ends here
