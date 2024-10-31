;;; v-ivy.el --- Vision's ivy package  -*- lexical-binding: t -*-
;; Version: 20220904
;;; Commentary:
;;; Code:
(defvar v-counsel-map (make-sparse-keymap))

;;;; Config
(defvar v-ivy-config
  '(;; amx support can be found in file `swiper/counsel.el`
    (use-package amx)

    (use-package counsel
      :delight
      :general
      (vision-map "i" v-counsel-map)
      (v-counsel-map
       "r" #'counsel-rg
       "f" #'counsel-fzf
       "g" #'counsel-git
       "o" #'counsel-outline
       "p" #'counsel-git-grep
       "f" #'counsel-recentf
       "s" #'counsel-semantic-or-imenu))

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

    (use-package prescient
      :config
      (prescient-persist-mode))

    ;; Sort the ivy candidates by frequency
    (use-package ivy-prescient
      :custom
      (ivy-prescient-retain-classic-highlighting t)
      :hook (ivy-mode . ivy-prescient-mode))

    ;; This package should put before `ivy-mode', as it add hook to `ivy-mode-hook'.
    (use-package ivy-rich
      :custom
      (ivy-rich-parse-remote-buffer nil "Parsing remote is tool slow")
      :hook (ivy-mode . ivy-rich-mode))

    (use-package ivy-xref
      :custom
      (xref-show-xrefs-function 'ivy-xref-show-xrefs))

    (use-package ivy
      :delight
      :custom
      (ivy-count-format "(%d/%d) ")
      (ivy-height 12)
      :config
      (ivy-mode))

    (use-package swiper
      :bind ("C-s" . swiper))))

;;;###autoload
(defun v-ivy-config ()
  "Eval variable `v-ivy-config'."
  (interactive)
  (eval `(progn ,@v-ivy-config)))

(provide 'v-ivy)
;;; v-ivy.el ends here
