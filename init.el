;;; init.el --- Load the full configuration           -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 霞鹜文楷真好看, 写代码的心情更愉悦了.

;; - `v-pkg-docs'

;; Speedup startup
(let ((fnha-original file-name-handler-alist))
  (setq gc-cons-threshold most-positive-fixnum)
  (setq file-name-handler-alist nil)
  (run-with-idle-timer
   1 nil
   (lambda ()
     ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
     (setq gc-cons-threshold 134217728) ; 128MiB
     (setq file-name-handler-alist fnha-original)
     (message "gc-cons-threshold and file-name-handler-alist restored"))))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;;;; Modules

(require 'init-core)                    ; Must be loaded first

;; The benchmark should be placed at the top of init.el so that it covers all init code.
(use-package benchmark-init
  :ensure t
  :hook (after-init . #'benchmark-init/deactivate))

(defvar v-ui-hook nil "Hook run when ui init.")
(defvar v-editor-hook nil "Hook run when editor init.")
(v-with-idle-timer 1
  (run-hooks 'v-editor-hook))
(v-with-idle-timer 2
  (run-hooks 'v-ui-hook))


(require 'v-pkg (v-join-user-emacsd "v" "v-pkg.el"))
(use-package v-pkg :v-ensure)
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
(use-package v-org :v-ensure
  :defer 1
  :config (v-org-config))


(require 'init-file)
(require 'init-tools)
(require 'init-coding)
(require 'init-git)
(require 'init-langs)
(require 'init-editor)
(require 'init-ui)
(require 'init-highlight)
(require 'init-drafts)


(v-load doom-ui
        tabs
        treemacs)

(v-load dumb-jump
        flycheck
        hydra
        lsp)

(v-load elisp
        javascript
        html
        plantuml
        xml
        yaml)


(defun v-python ()
  "Load `v-python' package."
  (interactive)
  (quelpa '(v-python :fetcher file :path "~/.emacs.d/v/v-python.el"))
  (v-python-config))


;;;; Emacs

(v-ensure-package 'edit-list)


;;;; Package

;; Parallel fetching/byte-compiling
(use-package feather
  :disabled
  :defer 5
  :delight
  :config
  (feather-mode))


;;;; Input method

(use-package rime
  :if is-wsl
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
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
  :disabled
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

(defvar v-proxy-old nil
  "Old proxy config value.")

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
            (progn
              (setq v-proxy-old url-proxy-services)
              `(("http" . ,v-proxy-config)
                ("https" . ,v-proxy-config)))
          (setq url-proxy-services v-proxy-old))))


(defun v-proxy-unset ()
  "Unset proxy config."
  (interactive)
  (setq url-proxy-services nil))


;;;; Keyboard
;; Prompt to eval a locally relevant function, with hints and keybindings.
(use-package key-assist)


;;;;; Evil
(use-package evil
  :hook (v-editor . evil-mode)
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

(use-package evil-escape
  :hook evil-mode)

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))


;;; init.el ends here
