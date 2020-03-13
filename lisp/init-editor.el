;;; init-editor.el --- Config of common editor features
;;; Commentary:
;;; Code:

;;;; Autosave
(use-package focus-autosave-mode
  :delight
  :defer 3
  :config (focus-autosave-mode))


;;;; Buffer
(use-package ibuffer
  :bind ("C-x C-b" . #'ibuffer))


;;;; Editor server
(use-package server
  :defer 3
  :config
  (if (not (or (daemonp) (server-running-p)))
      (server-start)))


;;;; Paren edit
(use-package smartparens
  :delight
  :defer 3
  :config (smartparens-global-mode))


;;;; Region
(use-package expand-region
  :bind ("C-=" . #'er/expand-region))


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
