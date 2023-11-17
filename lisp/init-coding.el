;;; init-coding.el --- Coding stuff
;;; Commentary:
;;; Code:
(require 'init-core)


(v-defun lang-find-definition
         "Find definitions of the symbol under point.")
(v-defun lang-find-references
         "Find references of the symbol under point.")
(v-defun lang-find-apropos
         "Find all meaningful symbols that match PATTERN")
(v-defun lang-sort-imports
         "Sort the region imports, or if none is select, the buffer.")
(v-defun lang-format
         "Format the region, or if none is select, the buffer.")
(v-defun lang-rename
         "Rename the symbol (and all references to it) under point.")
(v-defun lang-execute-code-action
         "Execute code action.")


(setq v-lang-find-references-func #'xref-find-references)
(setq v-lang-find-definition-func #'xref-find-definitions)
(setq v-lang-find-apropos-func #'xref-find-apropos)


;; Deprecated as of 20230805, use apheleia instead.
(use-package format-all)
(use-package apheleia
  :delight " Ap"
  :defer 1
  :config
  (setf (alist-get 'protobuf-mode apheleia-mode-alist) 'clang-format)
  (apheleia-global-mode))


(v-defmodule docker
  (use-package docker)
  (use-package dockerfile-mode))


(v-defmodule dumb-jump
  (use-package dumb-jump
    :defer 4
    :custom
    (dumb-jump-selector 'ivy)
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))


(v-defmodule flycheck
  (use-package flycheck
    :defer 4
    :custom
    (flycheck-emacs-lisp-load-path 'inherit)
    :general
    (vision-map
     "fl" #'flycheck-list-errors
     "fn" #'flycheck-next-error
     "fp" #'flycheck-previous-error)
    :config
    (global-flycheck-mode)
    (with-eval-after-load 'popwin
      (push "*Flycheck errors*" popwin:special-display-config)))


  (use-package flycheck-posframe
    :unless use-lsp
    :hook (flycheck-mode . flycheck-posframe-mode)
    :custom
    (flycheck-posframe-position 'window-center)
    :config
    (flycheck-posframe-configure-pretty-defaults))

  (use-package sideline-flycheck
    :after (sideline flycheck)
    :custom
    (sideline-flycheck-show-checker-name t)
    :init
    (add-to-list 'sideline-backends-right #'sideline-flycheck)
    :hook
    (flycheck-mode . sideline-flycheck-setup)))


(v-defmodule lsp
  (use-package lsp-ivy)

  (use-package lsp-ui
    :custom
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-show-with-cursor t)
    (lsp-ui-sideline-show-code-actions nil "Use `sideline-lsp' instead")
    (lsp-ui-sideline-show-diagnostics nil "Use `sideline-flycheck' instead"))

  (use-package sideline-lsp
    :after (sideline lsp-mode)
    :init
    (add-to-list 'sideline-backends-right #'sideline-lsp))

  (use-package lsp-mode
    :delight
    :custom
    (lsp-headerline-breadcrumb-enable t)
    (lsp-headerline-breadcrumb-icons-enable (display-graphic-p))
    :hook
    ((elixir-mode
      fsharp-mode
      go-mode
      js-mode
      nim-mode
      ruby-mode)
     . lsp-deferred)
    :config
    ;; Increase the amount of data which Emacs reads from the process.
    ;; Again the emacs default is too low 4k considering that
    ;; the some of the language server responses are in 800k - 3M range.
    (setq read-process-output-max (* 1024 1024)) ; 1MiB
    (general-def "C-c ;" lsp-command-map)))


(v-defmodule restclient
  (use-package restclient
    :init
    (with-eval-after-load 'evil
      (add-to-list 'evil-normal-state-modes 'restclient-mode)))

  (use-package ob-restclient))


(provide 'init-coding)
;;; init-coding.el ends here
