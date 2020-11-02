;;; init.el --- Load the full configuration           -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Speedup startup
(let ((gct-original gc-cons-threshold)
      (fnha-original file-name-handler-alist))
  (setq gc-cons-threshold (* 1024 1024 27))
  (setq file-name-handler-alist nil)
  (run-with-idle-timer
   4 nil
   (lambda ()
     (setq gc-cons-threshold gct-original)
     (setq file-name-handler-alist fnha-original)
     (message "gc-cons-threshold and file-name-handler-alist restored"))))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

;;;; Modules

(require 'init-core)                    ; Must be loaded first
(require 'v-utils)
(require 'init-tools)
(require 'init-coding)
(require 'init-langs)
(require 'init-editor)
(require 'init-ui)
(require 'init-org)
(require 'v-tests)


(v-load doom-ui treemacs tabs
        company dumb-jump flycheck git lsp
        restclient
        elisp javascript org html xml yaml)

(require 'v-modules)


(bootstrap :ui
           auto-theme
           highlight
           :tools
           avy
           ivy)


(when is-wsl
  (bootstrap :os wsl))


(unless window-system
  (bootstrap :os tty))


(load-file v-autoload-file)


(quelpa '(v :fetcher file :path "~/.emacs.d/v"))


(defun bootstrap2 ()
  (v-golang))


(add-hook 'emacs-startup-hook #'bootstrap2)


;;; init.el ends here
