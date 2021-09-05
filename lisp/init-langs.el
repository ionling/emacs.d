;;; init-langs.el --- Load languages
;;; Commentary:
;;; Code:
(require 'init-core)


(defvar v-lang-map (make-sparse-keymap))

(general-def v-lang-map
  "d" #'v-lang-find-definition
  "r" #'v-lang-find-references
  "a" #'v-lang-find-apropos
  "s" #'v-lang-sort-imports
  "f" #'v-lang-format)


(v-init python :package python)


(delight 'prettier-js-mode " Pr" 'prettier-js)


(v-defmodule elisp
  (use-package nameless
    :hook (emacs-lisp-mode . nameless-mode)))


(v-defmodule elixir
  (use-package elixir-mode))


(v-defmodule html
  (use-package prettier-js
    :hook
    (mhtml-mode . prettier-js-mode)))


(v-defmodule hy
  (use-package hy-mode
    :init
    (with-eval-after-load 'eldoc-box
      (add-hook 'hy-mode-hook #'eldoc-box-hover-at-point-mode))))


(v-defmodule javascript
  (use-package prettier-js
    :hook
    (js-mode . prettier-js-mode))

  (use-package js
    :custom
    (js-indent-level 2 "Keep same to prettier")))


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


(v-defmodule xml
  (use-package prettier-js
    :hook
    (nxml-mode . prettier-js-mode)))


(v-defmodule yaml
  (use-package prettier-js
    :hook
    (yaml-mode . prettier-js-mode))

  (use-package yaml-mode))


(provide 'init-langs)
;;; init-langs.el ends here
