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
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package avy-zap
  :general
  (v-avy-map "z" #'avy-zap-to-char-dwim))


(use-package ace-pinyin
  :defer 2
  :delight
  :config (ace-pinyin-global-mode))
