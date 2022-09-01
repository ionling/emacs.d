;;; v-golang.el --- vision golang config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration golang
;; Version: 20220901
;; Package-Requires: (go-mode golint gotest go-playground)

;;; Commentary:

;;; Code:

(require 'go-playground)
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
        (before-save . gofmt-before-save)
        (outline-minor-mode . v-golang-outline-set-local))

      (use-package golint)

      (use-package gotest)

      (use-package go-playground))))


(defun v-go-playground-browse (paste)
  "Browse selected PASTE."
  (interactive
   (->> go-playground-basedir
     f-directories
     (-map (lambda (d) (f-base d)))
     (completing-read "Paste: ")
     list))

  (find-file (f-join go-playground-basedir paste "snippet.go")))


(defun v-go-playground-count ()
  "Show go playground pastes count."
  (interactive)
  (->> go-playground-basedir
    f-directories
    length
    (message "Total %s pastes")))


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


;;;; Outline
(defvar v-golang-outline-comment-regex
  (rx "//" (group (+ "/")) " "))

(defun v-golang-outline-level ()
  "Go mode variable `outline-level' function."
  (let ((len (- (match-end 0) (match-beginning 0))))
    (cond ((looking-at v-golang-outline-comment-regex)
           (- (match-end 1) (match-beginning 1)))
          ;; Above should match everything but just in case.
          (t len))))

(defun v-golang-outline-set-local ()
  "Set `go-mode' `outline-regexp'."
  (setq-local outline-level #'v-golang-outline-level)
  (setq-local outline-regexp v-golang-outline-comment-regex))


(provide 'v-golang)
;;; v-golang.el ends here
