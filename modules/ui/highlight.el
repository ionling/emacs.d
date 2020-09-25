(use-package beacon
  :defer 6
  :delight
  :config
  (beacon-mode))


(use-package color-identifiers-mode
  :defer 6
  :delight
  :config
  (global-color-identifiers-mode))


(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))


(use-package highlight-parentheses
  :defer 6
  :delight
  :config (global-highlight-parentheses-mode))


(use-package hl-line :ensure nil
  :defer 6
  :config (global-hl-line-mode))


(use-package hl-todo
  :defer 6
  :config
  (add-to-list 'hl-todo-keyword-faces '("REF" . "#20abb0"))
  (global-hl-todo-mode))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package symbol-overlay
  :defer 2
  :delight
  :hook (prog-mode . symbol-overlay-mode)
  :general
  (symbol-overlay-map "c" #'symbol-overlay-count)
  :config
  (general-def 'vision-map "s" symbol-overlay-map))
