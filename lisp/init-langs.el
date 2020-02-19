;;; init-langs.el --- Load languages
;;; Commentary:
;;; Code:

(v-defmodule elisp
  (use-package nameless
    :hook (emacs-lisp-mode . nameless-mode)))


(v-defmodule elixir
  (use-package elixir-mode))


(v-defmodule hy
  (use-package hy-mode
    :init
    (with-eval-after-load 'eldoc-box
      (add-hook 'hy-mode-hook #'eldoc-box-hover-at-point-mode))))


(v-defmodule jsx
  (use-package rjsx-mode
    :init
    (with-eval-after-load 'org-src
      (push '("jsx" . rjsx) org-src-lang-modes))))


(v-defmodule ledger
  (use-package ledger-mode)

  (use-package flycheck-ledger
    :init
    (with-eval-after-load 'ledger-mode
      (require 'flycheck-ledger))))


(v-defmodule raku
  (use-package perl6-mode
    :init
    (defalias 'raku-mode #'perl6-mode)
    :mode ("\\.raku\\'" . raku-mode)))


(provide 'init-langs)
;;; init-langs.el ends here
