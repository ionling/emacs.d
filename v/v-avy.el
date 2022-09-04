;;; v-avy.el --- Vision's avy package  -*- lexical-binding: t -*-
;; Version: 20220904
;;; Commentary:
;;; Code:
(defvar v-avy-map (make-sparse-keymap))


;;;; Config
(defvar v-avy-config
  '((use-package avy
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
      :config (ace-pinyin-global-mode))))

;;;###autoload
(defun v-avy-config ()
  "Eval variable `v-avy-config'."
  (interactive)
  (eval `(progn ,@v-avy-config)))

(provide 'v-avy)
;;; v-avy.el ends here
