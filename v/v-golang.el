;;; v-golang.el --- vision golang config           -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'popwin)
(require 'projectile)
(require 'use-package)


;;;###autoload
(defun v-golang ()
  "Load golang config."
  (interactive)

  (use-package go-mode
    :custom
    (gofmt-command "goimports")
    :hook
    (before-save . gofmt-before-save))

  (use-package golint)

  (use-package gotest)

  (use-package go-playground))


;;;###autoload
(defun v-go-swag-init ()
  "Call `swag init` in project root."
  (interactive)
  ;; `shell-command' uses the the buffer's `default-directory' as working directory
  (let ((default-directory (projectile-project-root)))
    (shell-command "swag init" "*swag init*")))


;;;###autoload
(defun v-golangci-lint ()
  "Call `golangci-lint run` in project root."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (buffer-name "*golangci-lint*"))
    (shell-command "golangci-lint run" buffer-name)
    (popwin:popup-buffer buffer-name)))


(provide 'v-golang)
;;; v-golang.el ends here
