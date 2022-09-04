;;; v-auto-theme.el --- Vision's auto-theme package  -*- lexical-binding: t -*-
;; Version: 20220904
;;; Commentary:
;;; Code:
(require 'use-package)

(defcustom v-dark-theme 'doom-one
  "Default dark theme."
  :type '(symbol)
  :group 'v-ui)

(defcustom v-light-theme 'doom-one-light
  "Default light theme."
  :type '(symbol)
  :group 'v-ui)

;;;; Config
(defvar v-auto-theme-config
  '((use-package solar :ensure nil
      :custom
      ;; Get them at https://www.latlong.net
      (calendar-latitude 30.572815)
      (calendar-longitude 104.066803))

    (use-package circadian
      :if window-system
      :custom
      (circadian-themes `((:sunrise . ,v-light-theme)
                          (:sunset  . ,v-dark-theme)))
      :hook (emacs-startup . circadian-setup))))

;;;###autoload
(defun v-auto-theme-config ()
  "Eval variable `v-auto-theme-config'."
  (interactive)
  (eval `(progn ,@v-auto-theme-config)))

(provide 'v-auto-theme)
;;; v-auto-theme.el ends here
