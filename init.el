;; Speed up startup
(setq garbage-collection-messages t)
(defvar default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; amx support can be found in file `swiper/counsel.el`
(use-package amx)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy))

(use-package company
  :diminish "C"
  :hook (after-init . global-company-mode)
  :config (setq company-idle-delay 0.1))

(use-package company-lsp
  :config (push 'company-lsp company-backends))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package ws-butler
  :config (ws-butler-global-mode))

(use-package company-statistics
  :config (company-statistics-mode))

(use-package counsel
  :diminish
  :config (counsel-mode))

(use-package undo-tree
  :diminish "UT")

(use-package restart-emacs)

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode))

(use-package evil
  ;; Note: You should enable global-evil-leader-mode before you enable evil-mode,
  ;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, …).
  :after evil-leader
  :config
  ;;(evil-define-key 'normal evil-motion-state-map (kbd "C-i") nil)
  (evil-mode))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'org-brain-visualize
    "y" 'youdao-dictionary-search-at-point
    "SPC" 'ace-jump-mode
    "jj" 'dumb-jump-go
    "jo" 'dumb-jump-go-other-window
    "jb" 'dumb-jump-back
    )
  (global-evil-leader-mode))

(use-package highlight-parentheses
  :diminish
  :config
  (global-highlight-parentheses-mode))

(use-package indent-guide
  :config (indent-guide-global-mode))

(use-package ivy
  :diminish
  :config
  (ivy-mode)
  (setq ivy-count-format "(%d/%d) "))

(use-package olivetti
  ;; :hook (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

;; Python
(use-package anaconda-mode
  :after python
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-function
	(lambda ()
	  (format " P[%s]"
		  (if (< (length projectile-project-name) 5)
		      (projectile-project-name)
		    (concat (substring projectile-project-name 0 4) "...")))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package lsp-mode
  :config
  (lsp-define-stdio-client lsp-python "python"
    (lsp-make-traverser #'(lambda (dir)
                (directory-files
                dir
                nil
                "\\(__init__\\|setup\\)\\.py\\|Pipfile")))

    '("pyls")
    :docstring "define"))

(add-hook 'python-mode-hook (lambda () (lsp-python-enable)))

(use-package neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-smart-open t))

(use-package smartparens
  :config (smartparens-global-mode))

(use-package spaceline
  :config
  (spaceline-spacemacs-theme)
  (setq powerline-default-separator 'wave)
  (spaceline-compile))

(use-package swiper
  :config (global-set-key (kbd "C-s") 'swiper))

(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (delete 'newline-mark whitespace-style)
  (delete 'spaces whitespace-style)
  (delete 'space-mark whitespace-style))

(use-package window-numbering
  :config (window-numbering-mode))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package youdao-dictionary)

(use-package zoom
  :config
  (setq zoom-size '(80 . 28))
  (zoom-mode))

(use-package paradox)

(use-package ace-jump-mode)

(use-package flyspell
  :diminish "FS"
  :hook (prog-mode . flyspell-prog-mode)
  :config
  (setq ispell-dictionary "en"))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-org)

;; Terminal
(when (eq window-system nil))


(setq default-frame-alist
'((font . "Hack-11")
  (fullscreen . maximized)))



(global-hl-line-mode)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(window-divider-mode 0)

(if (not (or (daemonp) (server-running-p)))
    (server-start))

(setq gc-cons-threshold default-gc-cons-threshold)
