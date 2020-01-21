;;; init-ui.el --- UI configuration
;;; Commentary:
;;; Code:
(use-package all-the-icons)


(v-defmodule doom-ui
  (use-package doom-themes
    :config
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon t)
    (doom-modeline-minor-modes t)
    :hook (after-init . doom-modeline-mode)))


(v-defmodule highlight
  (use-package highlight-parentheses
    :delight
    :init (global-highlight-parentheses-mode))

  (use-package hl-line
    :init (global-hl-line-mode))

  (use-package hl-todo
    :init (global-hl-todo-mode))

  (use-package symbol-overlay
    :defer 2
    :delight
    :general
    (symbol-overlay-map "c" #'symbol-overlay-count)
    :config
    (general-def 'vision-map "s" symbol-overlay-map)
    (symbol-overlay-mode)))


(v-defmodule tabs
  (use-package centaur-tabs
    :custom
    (centaur-tabs-close-button "✕")
    (centaur-tabs-set-bar 'left)
    :init (centaur-tabs-mode)))


(provide 'init-ui)
;;; init-ui.el ends here
