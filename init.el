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
(require 'v-pkg (v-join-user-emacsd "v" "v-pkg.el"))
(require 'v-utils)
(require 'init-tools)
(require 'init-coding)
(require 'init-git)
(require 'init-langs)
(require 'init-editor)
(require 'init-ui)
(require 'init-org)
(require 'init-highlight)
(require 'v-tests)


(v-load doom-ui treemacs tabs
        dumb-jump flycheck lsp
        restclient
        elisp javascript org html xml yaml)


(defvar v-init-golang nil "Golang config already inited.")

(defun v-init-golang ()
  "Init golang config."
  (unless v-init-golang
    (v-ensure-package 'v-golang)
    (v-golang-config)
    (setq v-init-golang t)))

(with-eval-after-load 'go-mode
  (advice-add 'go-mode :before #'v-init-golang))


(defun v-python ()
  "Load `v-python' package."
  (interactive)
  (quelpa '(v-python :fetcher file :path "~/.emacs.d/v/v-python.el"))
  (v-python-config))

(use-package v-file :v-ensure)
(use-package v-demo :v-ensure)
(use-package v-graphic :v-ensure)
(use-package v-text :v-ensure)
(use-package v-unknown :v-ensure)
(use-package v-avy :v-ensure
  :defer .3
  :config (v-avy-config))
(use-package v-ivy :v-ensure
  :defer .3
  :config (v-ivy-config))
(use-package v-complete :v-ensure
  :defer .4
  :config (v-complete-config))
(use-package v-auto-theme :v-ensure
  :defer .5
  :config (v-auto-theme-config))
(use-package v-wsl :v-ensure
  :if is-wsl
  :defer .5
  :config (v-wsl-config))


;;;; Emacs

(v-ensure-package 'edit-list)


;;;; Package

;; Parallel fetching/byte-compiling
(use-package feather
  :defer 5
  :delight
  :config
  (feather-mode))


;;;; Input method

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe))


(use-package pyim
  :disabled
  :defer 1
  :if (and window-system is-wsl)
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'microsoft-shuangpin)
  (pyim-page-tooltip 'posframe "Better performance than popup")
  :config
  ;; 使用半角标点
  (setq-default pyim-punctuation-translate-p '(no yes auto))

  ;; 使 pyim 在 ivy-posframe 中能够显示候选词
  ;; https://github.com/tumashu/ivy-posframe/issues/80#issuecomment-683519783
  (with-eval-after-load 'ivy-posframe
    (setq ivy-posframe-hide-minibuffer nil)))


(use-package pyim-basedict
  :after pyim
  :demand t
  :config
  (pyim-basedict-enable))


;;;; Graphic

(when (display-graphic-p)
  (add-hook 'kill-emacs-hook #'v-frame-geometry-save))


;;;; Network

(defcustom v-proxy-config ""
  "The proxy config: a string of host:port."
  :type 'string
  :group 'v-proxy)

(defun v-proxy-config ()
  "Show current proxy config."
  (interactive)
  (message "Proxy config: %s" v-proxy-config))

(define-minor-mode v-proxy-mode
  "A mode for HTTP/HTTPS proxy.
Refer https://emacs-china.org/t/topic/2808/24."
  :global t
  :group 'v-proxy
  :init-value nil
  :lighter " Proxy"
  (setq url-proxy-services
        (if v-proxy-mode
            `(("http" . ,v-proxy-config)
              ("https" . ,v-proxy-config)))))


;;;; Keyboard
;;;;; Evil
(use-package evil
  :hook (emacs-startup . evil-mode)
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (dolist (key '("C-a" "C-e" "C-n" "C-p" "C-d" "C-y"))
    (define-key evil-insert-state-map (kbd key) nil))
  (dolist (key '("C-e" "C-i"))
    (define-key evil-motion-state-map (kbd key) nil))

  (delete 'compilation-mode evil-motion-state-modes)
  (setq evil-default-state 'emacs)
  (setq evil-normal-state-modes
        (-union evil-normal-state-modes
                '(prog-mode text-mode org-mode conf-mode yaml-mode))))

(use-package evil-surround
  :defer 4
  :config
  (global-evil-surround-mode))


;;; init.el ends here
