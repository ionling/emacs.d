(defvar v-counsel-map (make-sparse-keymap))


;; amx support can be found in file `swiper/counsel.el`
(use-package amx)


(use-package ivy
  :delight
  :custom
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))


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
  :hook (emacs-startup . counsel-mode)
  :general
  (vision-map "c" v-counsel-map)
  (v-counsel-map "r" #'counsel-rg
                 "f" #'counsel-fzf
                 "g" #'counsel-git
                 "o" #'counsel-outline
                 "p" #'counsel-git-grep
                 "f" #'counsel-recentf
                 "s" #'counsel-semantic-or-imenu))
