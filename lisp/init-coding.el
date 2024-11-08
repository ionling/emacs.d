;;; init-coding.el --- Coding stuff
;;; Commentary:
;;; Code:
(require 'dash)

(require 'init-core)
(require 'init-modules)


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


(use-package xref
  :custom
  (xref-prompt-for-identifier nil)
  :init
  (setq v-lang-find-references-func #'xref-find-references)
  (setq v-lang-find-definition-func #'xref-find-definitions)
  (setq v-lang-find-apropos-func #'xref-find-apropos))


(use-package editorconfig
  :delight
  :hook (after-init . editorconfig-mode))


;;;; ElDoc
(use-package eldoc :delight)

(use-package eldoc-box
  :delight eldoc-box-hover-at-point-mode
  :hook (emacs-lisp-mode . eldoc-box-hover-at-point-mode))


;;;; Format

;; Deprecated as of 20230805, use apheleia instead.
(use-package format-all :disabled)
(use-package apheleia
  :delight " Ap"
  :defer 1
  :config
  (setf (alist-get 'protobuf-mode apheleia-mode-alist) 'clang-format)
  (push '(sql-formatter . ("npx sql-formatter")) apheleia-formatters)
  (apheleia-global-mode))


;;;; Snippets
(use-package yasnippet
  :defer 2
  :delight yas-minor-mode
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet :if v-mod-ivy-enabled)

;;;; Misc

(use-package hideshow
  :delight hs-minor-mode
  :hook
  (prog-mode . hs-minor-mode))

(use-package imenu-list
  :general
  (v-lang-map "i" #'imenu-list)
  :custom
  (imenu-list-auto-resize t))

(use-package import-popwin)


;;;; Modules

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
    :defer 1
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
    :unless v-mod-lsp-enabled
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
  (defun v-lsp-workspace-remove-all ()
    "Remove all lsp workspaces."
    (interactive)
    (--each (lsp-session-folders (lsp-session))
      (lsp-workspace-folders-remove it)))

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

  (defun v-coding-set-actions ()
    "Change coding actions to LSP."
    (setq-local v-lang-format-func #'lsp-format-buffer)
    (setq-local v-lang-rename-func #'lsp-rename)
    (setq-local v-lang-execute-code-action-func #'lsp-execute-code-action))

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
    :init
    (add-hook 'lsp-mode-hook #'v-coding-set-actions)
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


(v-defmodule treemacs
  (use-package treemacs
    :general
    (vision-file-map "t" #'treemacs))

  (use-package lsp-treemacs :if v-mod-lsp-enabled)

  (use-package treemacs-projectile))


;;;; Disabled

(use-package semantic
  :disabled
  :hook (prog-mode . semantic-mode)
  :custom
  (semantic-default-submodes
   '(global-semantic-decoration-mode
     global-semantic-highlight-edits-mode
     global-semantic-highlight-func-mode
     global-semantic-idle-breadcrumbs-mode
     global-semantic-idle-scheduler-mode
     global-semantic-mru-bookmark-mode
     global-semanticdb-minor-mode)))


(provide 'init-coding)
;;; init-coding.el ends here
