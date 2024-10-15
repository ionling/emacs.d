;;; init-langs.el --- Load languages
;;; Commentary:
;;; Code:
(require 'init-core)
(require 'init-coding)


(defvar v-lang-map (make-sparse-keymap))

(general-def v-lang-map
  "d" #'v-lang-find-definition
  "r" #'v-lang-find-references
  "a" #'v-lang-find-apropos
  "s" #'v-lang-sort-imports
  "f" #'v-lang-format
  "n" #'v-lang-rename
  "e" #'v-lang-execute-code-action)


(v-init python :package python)

;;;; Golang

(defvar v-init-golang nil "Golang config already inited.")

(defun v-init-golang ()
  "Init golang config."
  (unless v-init-golang
    (v-ensure-package 'v-golang)
    (v-golang-config)
    (setq v-init-golang t)))

(v-init-register-ext 'go #'v-init-golang)


(delight 'prettier-js-mode " Pr" 'prettier-js)


(v-defmodule elisp
  (use-package nameless
    :delight
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

  (use-package js :ensure nil
    :custom
    (js-indent-level 2 "Keep same to prettier")))


(v-defmodule jsx
  (use-package rjsx-mode
    :init
    (with-eval-after-load 'org-src
      (push '("jsx" . rjsx) org-src-lang-modes))))

(use-package fsharp-mode :module fsharp)
(use-package haskell-mode :module haskell)
(use-package racket-mode :module racket)

(v-defmodule ruby
  (use-package enh-ruby-mode)
  (use-package robe))

(v-defmodule ledger
  (use-package ledger-mode)

  (use-package flycheck-ledger
    :init
    (with-eval-after-load 'ledger-mode
      (require 'flycheck-ledger))))


(v-defmodule raku
  (use-package raku-mode
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


(v-defmodule plantuml
  (use-package plantuml-mode
    :custom
    (plantuml-default-exec-mode 'executable)
    (plantuml-indent-level 4)))

;;;; Protocol Buffers
(defun v-protobuf-config ()
  "Load Protocol Buffers config."
  (use-package protobuf-mode
    :init
    (with-eval-after-load 'evil
      (defvar evil-normal-state-modes)
      (add-to-list 'evil-normal-state-modes 'protobuf-mode))
    (with-eval-after-load 'aggressive-indent
      (defvar aggressive-indent-excluded-modes)
      (add-to-list 'aggressive-indent-excluded-modes 'protobuf-mode)))

  (protobuf-mode))


(v-init-register-ext 'proto #'v-protobuf-config)


;;;; sh
(defvar v-sh-outline-regex
  (rx "#" (group (+ "#")) " "))

(defun v-sh-outline-level ()
  "Shell-script mode variable `outline-level' function."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (cond ((looking-at v-sh-outline-regex)
           (- (match-end 1) (match-beginning 1)))
          (t len))))

(defun v-sh-outline-set-local ()
  "Set `sh-mode' buffer-local variables."
  (when outline-minor-mode
    (setq-local outline-level #'v-sh-outline-level)
    (setq-local outline-regexp v-sh-outline-regex)))

(use-package sh-script
  :hook
  (sh-mode . v-sh-outline-set-local))


(provide 'init-langs)
;;; init-langs.el ends here
