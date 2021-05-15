;;; init-editor.el --- Config of common editor features
;;; Commentary:
;;; Code:

(defcustom v-file-save-place t
  "Automatically save place in each file and go to the place at next visiting."
  :type 'boolean
  :group 'v-file)


;;;; Better defaults
(set-default 'fill-column 80)
(setq column-number-indicator-zero-based nil)
(setq make-backup-files nil)


;;;; Autosave
(use-package focus-autosave-mode
  :delight
  :defer 3
  :config (focus-autosave-mode))


;;;; Buffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


(use-package saveplace
  :if v-file-save-place
  :defer 2
  :config
  (save-place-mode))


;;;; Editor server
(use-package server
  :defer 3
  :config
  (if (not (or (daemonp) (server-running-p)))
      (server-start)))


;;;; Fill paragraph
(use-package aggressive-fill-paragraph
  :hook (org-mode . aggressive-fill-paragraph-mode))


;;;; Indent
(set-default 'indent-tabs-mode nil)

(use-package aggressive-indent
  :defer 3
  :config (global-aggressive-indent-mode))


;;;; Paren edit
(use-package smartparens
  :delight
  :defer 3
  :config
  (require 'smartparens-config)         ; Load default config
  (smartparens-global-mode)
  ;; https://i.loli.net/2020/12/28/SIGCO29ZDX6wBYl.gif
  (show-smartparens-global-mode))


;;;; Region
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;;;; Scrolling
(use-package smooth-scrolling
  :defer 3
  :config (smooth-scrolling-mode))


;;;; Whitespace
(use-package ws-butler
  :delight
  :defer 3
  :config (ws-butler-global-mode))


(provide 'init-editor)
;;; init-editor.el ends here
