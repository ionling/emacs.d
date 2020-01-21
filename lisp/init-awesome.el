;;; init-awesome.el --- Init awesome packages
;;; Commentary:
;;; Code:

(v-defmodule avy
  (defvar v-avy-map (make-sparse-keymap))

  (use-package avy
    :general
    (vision-map "SPC" #'avy-goto-word-1
                "a" v-avy-map)
    (v-avy-map "i" #'avy-goto-char-in-line
               "l" #'avy-goto-line))

  (use-package ace-link
    :general
    (v-avy-map "j" #'ace-link))

  (use-package ace-window
    :bind
    ("M-o" . ace-window)
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

  (use-package avy-zap
    :general
    (v-avy-map "z" #'avy-zap-to-char-dwim))

  (use-package ace-pinyin
    :delight
    :init (ace-pinyin-global-mode)))


(v-defmodule ivy
  (defvar v-counsel-map (make-sparse-keymap))

  ;; amx support can be found in file `swiper/counsel.el`
  (use-package amx)

  (use-package ivy
    :delight
    :config
    (ivy-mode)
    (setq ivy-count-format "(%d/%d) "))

  (use-package ivy-rich
    :hook (ivy-mode . ivy-rich-mode))

  (use-package ivy-posframe
    :delight
    :hook (ivy-mode . ivy-posframe-mode)
    :config
    (setq ivy-posframe-parameters
          '((left-fringe . 12) (right-fringe . 12))
          ivy-posframe-height-alist
          '((counsel-M-x . 18)
            (t . 16))
          ivy-posframe-display-functions-alist
          '((swiper . ivy-posframe-display-at-point)
            (t . ivy-posframe-display-at-frame-center))))

  (use-package swiper
    :bind ("C-s" . swiper))

  (use-package counsel
    :delight
    :hook (after-init . counsel-mode)
    :general
    (vision-map "c" v-counsel-map)
    (v-counsel-map "r" #'counsel-rg
                   "f" #'counsel-fzf
                   "g" #'counsel-git
                   "o" #'counsel-outline
                   "p" #'counsel-git-grep
                   "f" #'counsel-recentf
                   "s" #'counsel-semantic-or-imenu
                   )))

(provide 'init-awesome)
;;; init-awesome.el ends here
