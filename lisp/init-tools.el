;;; init-tools.el --- Load some useful tools
;;; Commentary:
;;; Code:

(use-package google-this
  :general
  (v-point-map "g" #'google-this))


(use-package wakatime-mode
  :delight
  :defer 8
  :config (global-wakatime-mode))


(use-package youdao-dictionary
  :general
  (v-point-map "y" #'youdao-dictionary-search-at-point-posframe))


(use-package paradox
  :defer 8
  :config
  (paradox-enable))


(use-package undo-tree
  :delight)


(provide 'init-tools)
;;; init-tools.el ends here
