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

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package company
  :hook (after-init . global-company-mode)
  :config (setq company-idle-delay 0.1))

(use-package company-lsp
  :config (push 'company-lsp company-backends))

(use-package company-statistics
  :config (company-statistics-mode))

(use-package counsel
  :config (counsel-mode))

(use-package diminish
  :after
  (highlight-parentheses indent-guide ivy)
  :config
  (diminish 'company-mode "C")
  (diminish 'counsel-mode)
  (diminish 'eldoc-mode "E")
  (diminish 'highlight-parentheses-mode)
  (diminish 'indent-guide-mode)
  (diminish 'ivy-mode)
  (diminish 'lsp-mode "L")
  (diminish 'undo-tree-mode "UT"))

(use-package evil
  :config
  ;;(evil-define-key 'normal evil-motion-state-map (kbd "C-i") nil)
  (evil-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode))

(use-package indent-guide
  :config (indent-guide-global-mode))

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-count-format "(%d/%d) "))

(use-package olivetti
  ;; :hook (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

;;(use-package org)
  ;;:config (define-key org-mode-map (kbd "TAB") 'org-cycle))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

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
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-right)
  (which-key-mode))

;; Terminal
(when (eq window-system nil)
  )

;; GUI mode
(when window-system
  ;; Window width
  (set-frame-width (selected-frame) 130))


;; Font
;; 等距更纱黑体 SC
;; Fira Code
;; 方正黑体_GBK
;; STFangsong
;; Noto Mono
;; Noto Sans Mono
;; MF JiaHei (Noncommercial)
;; MF ManYu (Noncommercial)
(set-frame-font "Fira Code-11")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "Source Han Sans SC" :size 15)))

(global-hl-line-mode +1)
(tool-bar-mode -1)

;; trailing whitespace
(setq-default show-trailing-whitespace t)
;; Refactor with dolist
(add-hook 'buffer-menu-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eww-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(server-start)

(setq gc-cons-threshold default-gc-cons-threshold)
