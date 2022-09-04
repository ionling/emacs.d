;;; v-wsl.el --- Vision's WSL package  -*- lexical-binding: t -*-
;; Version: 20220904
;; Package-Requires: (dash s)
;;; Commentary:
;;; Code:
(require 's)
(require 'dash)


(defvar wsl-host
  (-> "cat /etc/resolv.conf | grep nameserver | awk '{print $2}'"
      (shell-command-to-string)
      (s-trim))
  "WSL Host.")

;; https://www.reddit.com/r/emacs/comments/6xryqh/emacs_in_wsl_and_the_windows_clipboard/
;;;###autoload
(defun wsl-copy (start end)
  "Copy START to END region into Windows clipboard."
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (message "Copied: %s" (buffer-substring start end)))


;;;; Config
(defvar v-wsl-config
  '((with-eval-after-load 'browse-url
      (setq browse-url-generic-program "cmd.exe"
            browse-url-generic-args '("/c" "start")
            browse-url-browser-function 'browse-url-generic))))

;;;###autoload
(defun v-wsl-config ()
  "Eval variable `v-wsl-config'."
  (interactive)
  (eval `(progn ,@v-wsl-config)))

(provide 'v-wsl)
;;; v-wsl.el ends here
