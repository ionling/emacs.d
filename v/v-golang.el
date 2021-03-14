;;; v-golang.el --- vision golang config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration golang
;; Package-Requires: (go-mode golint gotest go-playground)

;;; Commentary:

;;; Code:

(require 'popwin)
(require 'projectile)
(require 'use-package)


;;;###autoload
(defun v-golang-config ()
  "Load golang config."
  (interactive)
  (eval
   '(progn
      (use-package go-mode
        :custom
        (gofmt-command "goimports")
        :hook
        (before-save . gofmt-before-save))

      (use-package golint)

      (use-package gotest)

      (use-package go-playground))))


(defun v-go-swag-init ()
  "Call `swag init` in project root."
  (interactive)
  ;; `shell-command' uses the the buffer's `default-directory' as working directory
  (let ((default-directory (projectile-project-root)))
    (shell-command "swag init" "*swag init*")))


(defun v-golangci-lint ()
  "Call `golangci-lint run` in project root."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (buffer-name "*golangci-lint*"))
    (shell-command "golangci-lint run" buffer-name)
    (popwin:popup-buffer buffer-name)))


(defun v-go-shorten-lines ()
  "Shorten long lines in current file."
  (interactive)
  (save-buffer)
  (shell-command
   (format "golines --no-reformat-tags -m %s -w %s"
           fill-column buffer-file-name)))


(provide 'v-golang)
;;; v-golang.el ends here
