;;; init-drafts.el --- Some drafts here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-core)
(require 'use-package)

;; Started at <2023-03-04 Sat>
(use-package auto-package-update
  :disabled
  :defer 1
  :config
  (auto-package-update-maybe))

(when nil
  (setq doom-modeline-icon nil)

  ;; Not working in WSL2 Windows Terminal
  (use-package evil-terminal-cursor-changer :disabled)

  ;; Not working with TMUX and Alacritty
  (use-package clipetty :disabled))
(provide 'init-drafts)
;;; init-drafts.el ends here
