;;; v-python.el --- vision python config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration
;; Package-Requires: (python python-black pipenv py-isort lsp-mode lsp-python-ms)

;;; Commentary:

;;; Code:

(require 'lsp-python-ms)
(require 'py-isort)
(require 'python-black)
(require 'use-package)


;;;###autoload
(defun v-python-config ()
  "Load python config."
  (interactive)
  (eval
   '(progn
      (use-package python
        :config
        ;; https://stackoverflow.com/a/14033335/7134763
        (define-coding-system-alias 'UTF-8 'utf-8)
        (setq-local v-lang-format-func #'v-python-format
                    v-lang-sort-imports-func #'v-python-sort-imports))

      (use-package pipenv
        :delight " Pe"
        :hook (python-mode . pipenv-mode)
        :init
        (advice-add 'pipenv-activate :after #'v-python-fix-lsp-ms-cmd))

      (use-package py-isort)

      (use-package python-black
        :delight python-black-on-save-mode
        :hook (python-mode . python-black-on-save-mode))

      (use-package lsp-python-ms
        :hook
        (python-mode
         . (lambda ()
             (require 'lsp-python-ms)
             (lsp-deferred)))))))


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


(defun v-python-format ()
  "Format region, if region is active, otherwise buffer."
  (interactive)
  (if (region-active-p)
      (python-black-region (region-beginning) (region-end))
    (python-black-buffer)))


(defun v-python-sort-imports ()
  "Sort imports in region, if region is active, otherwise buffer."
  (interactive)
  (if (region-active-p)
      (py-isort-region)
    (py-isort-buffer)))


(defun v-python-fix-lsp-ms-cmd ()
  "Fix `lsp-python-ms-python-executable-cmd' when using pipenv."
  (setq-local lsp-python-ms-python-executable-cmd
              (concat python-shell-virtualenv-root "/bin/python")))


(provide 'v-python)
;;; v-python.el ends here
