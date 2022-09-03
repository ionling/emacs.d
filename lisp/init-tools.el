;;; init-tools.el --- Load some useful tools
;;; Commentary:
;;; Code:

(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :config
  (setq
   dashboard-banner-logo-title "Be who you are"
   dashboard-center-content  t
   dashboard-items '((projects . 4)
                     (recents  . 7)
                     (bookmarks . 4)
                     (registers . 4))
   initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))


(use-package google-this
  :general
  (v-point-map "h" #'google-this))


(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (("C-h f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h y" . helpful-at-point)))


(use-package wakatime-mode
  :delight
  :defer 8
  :config (global-wakatime-mode))


(use-package youdao-dictionary
  :init
  (defun v-youdao-dict-search-at-point ()
    "Automatically call best `youdao-dictionary-search-at-point-' function."
    (interactive)
    (cond ((and (require 'posframe nil t) (posframe-workable-p))
           ;; Copied from `youdao-dictionary--posframe-tip'
           (youdao-dictionary-search-at-point-posframe))
          ((display-graphic-p) (youdao-dictionary-search-at-point-tooltip))
          (t (youdao-dictionary-search-at-point+))))
  :general
  (v-point-map "y" #'v-youdao-dict-search-at-point))


(use-package paradox
  :defer 8
  :config
  (paradox-enable))


(use-package undo-tree
  :delight)


(provide 'init-tools)
;;; init-tools.el ends here
