;;; v-golang.el --- vision golang config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration golang
;; Version: 20241018
;; Package-Requires: (go-mode golint gotest go-gen-test go-playground
;;   f dash lsp popwin projectile)

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 'go-mode)
(require 'go-gen-test)
(require 'go-playground)
(require 'lsp)
(require 'popwin)
(require 'projectile)


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
        (outline-minor-mode . v-golang-outline-set-local)
        :init
        (with-eval-after-load 'evil
          (add-to-list 'evil-normal-state-modes 'go-dot-mod-mode)))

      (use-package go-direx :disabled)

      (use-package go-tag :disabled)

      (use-package golint :tags golang lint)

      (use-package gotest :tags golang test
        :config
        (setq-default go-test-args "-v")
        (push 'go-test-mode popwin:special-display-config))

      ;; Generate test with gotests
      (use-package go-gen-test :tags golang test)

      (use-package go-playground))))


;;;###autoload
(defun v-golang-mod-tidy ()
  "Run \"go mod tidy\"."
  (interactive)
  (shell-command "go mod tidy"))

;;;###autoload
(defun v-golang-playground-browse (paste)
  "Browse selected PASTE."
  (interactive
   (->> go-playground-basedir
        f-directories
        (-map (lambda (d) (f-base d)))
        (completing-read "Paste: ")
        list))

  (find-file (f-join go-playground-basedir paste "snippet.go")))

;;;###autoload
(defun v-golang-playground-count ()
  "Show go playground pastes count."
  (interactive)
  (->> go-playground-basedir
       f-directories
       length
       (message "Total %s pastes")))

;;;###autoload
(defun v-golang-playground-remove-lsp-workspaces ()
  "Remove lsp workspaces of `go-playground'."
  (interactive)
  (--each
      (--filter (s-prefix? (f-full "~/go/src/playground") it)
                (lsp-session-folders (lsp-session)))
    (lsp-workspace-folders-remove it)))

(defun v-golang-swag-init ()
  "Call `swag init` in project root."
  (interactive)
  ;; `shell-command' uses the the buffer's `default-directory' as working directory
  (let ((default-directory (projectile-project-root)))
    (shell-command "swag init" "*swag init*")))

(defun v-golang-swag-fmt ()
  "Call `swag fmt` in project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "swag fmt" "*swag fmt*")))

(defun v-golang-golangci-lint ()
  "Call `golangci-lint run` in project root."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (buffer-name "*golangci-lint*"))
    (shell-command "golangci-lint run" buffer-name)
    (popwin:popup-buffer buffer-name)))


(defun v-golang-shorten-lines ()
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

(defun v-golang-test-gen-at-point ()
  "Generate test for function at point.
Refer `go-gen-test-dwim'."
  (interactive)
  (save-excursion
    (go-goto-function-name)
    (shell-command
     (format "%s -only %s %s"
             (go-gen-test-base-command)
             (shell-quote-argument (thing-at-point 'symbol))
             (shell-quote-argument buffer-file-name)))
    (funcall go-gen-test-open-function)))


(provide 'v-golang)
;;; v-golang.el ends here
