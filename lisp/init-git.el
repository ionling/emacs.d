;;; init-git.el --- Git stuff
;;; Commentary:
;;; Code:
(require 'use-package)


(defvar v-git-map (make-sparse-keymap)
  "Git keymap.")


(use-package gitignore-mode)

(use-package git-messenger
  :general
  (v-git-map "g" #'git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t))


(use-package git-gutter
  :delight
  :defer 4
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 1)
  :general
  (v-git-map
   "m" #'git-gutter:mark-hunk
   "n" #'git-gutter:next-hunk
   "p" #'git-gutter:previous-hunk
   "d" #'git-gutter:popup-hunk ; diff
   "r" #'git-gutter:revert-hunk
   "s" #'git-gutter:stage-hunk)
  :config
  (global-git-gutter-mode))
  

(use-package vc :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-command-messages t))


(provide 'init-git)
;;; init-git.el ends here
