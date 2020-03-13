;;; init-langs.el --- Load languages
;;; Commentary:
;;; Code:
(defvar v-lang-map (make-sparse-keymap))

(v-defun find-definition "Find definitions of the symbol under point.")

(v-defun find-references "Find references of the symbol under point.")

(v-defun sort-imports "Sort the region imports, or if none is select, the buffer.")

(v-defun format "Format the region, or if none is select, the buffer.")


(general-def v-lang-map
  "d" #'v-find-definition
  "r" #'v-find-references
  "f" #'v-format
  "s" #'v-sort-imports)


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


(v-defmodule python
  (require 'mode-local)

  (use-package python
    :config
    ;; https://stackoverflow.com/a/14033335/7134763
    (define-coding-system-alias 'UTF-8 'utf-8))

  ;; From https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
  ;; layers/+lang/python/funcs.el
  ;; autoflake --remove-all-unused-imports -i target.py
  (defun spacemacs/python-remove-unused-imports ()
    "Use Autoflake to remove unused function."
    (interactive)
    (if (executable-find "autoflake")
        (progn
          (shell-command
           (format "autoflake --remove-all-unused-imports -i %s"
                   (shell-quote-argument (buffer-file-name))))
          (revert-buffer t t t))
      (message "Error: Cannot find autoflake executable.")))


;;;; Microsoft Python Language Server
  (require 'lsp-python-ms)

  (use-package lsp-python-ms
    :hook
    (python-mode
     . (lambda ()
         (require 'lsp-python-ms)
         (lsp)))
    ;; (lsp-deferred)))
    :config
    (setq-mode-local python-mode
                     v-find-references-func #'lsp-find-references
                     v-find-definition-func #'lsp-find-definition)

    (defun fix-python-cmd ()
      (setq-local lsp-python-ms-python-executable-cmd
                  (concat python-shell-virtualenv-root "/bin/python")))

    (advice-add 'pipenv-activate :after #'fix-python-cmd))


;;;; Black
  (use-package python-black
    :delight python-black-on-save-mode
    :hook
    ((python-mode . python-black-on-save-mode)
     (python-mode
      . (lambda ()
          (setq-local v-format-func #'format-python))))
    :init
    (defun format-python ()
      "Format region, if region is active, otherwise buffer."
      (interactive)
      (if (region-active-p)
          (python-black-region (region-beginning) (region-end))
        (python-black-buffer))))

;;;; Others
  (use-package pipenv
    :delight " Pe"
    :hook (python-mode . pipenv-mode))

  (use-package py-isort
    :init
    (defun sort-imports-python ()
      "Sort imports in region, if region is active, otherwise buffer."
      (interactive)
      (if (region-active-p)
          (py-isort-region)
        (py-isort-buffer))))

  (setq-mode-local python-mode v-sort-imports-func #'sort-imports-python))


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
