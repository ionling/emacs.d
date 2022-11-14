;;; init-file.el --- File related config
;;; Commentary:
;;; Code:
(require 'use-package)

;; Show you recently opened files
(use-package recentf
  :defer 1
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode))

(provide 'init-file)
;;; init-file.el ends here
