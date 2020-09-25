
(require 's)

(defvar wsl-host "" "WSL Host.")

(setq wsl-host
      (-> "cat /etc/resolv.conf | grep nameserver | awk '{print $2}'"
          (shell-command-to-string)
          (s-trim)))



(setq browse-url-generic-program "cmd.exe"
      browse-url-generic-args '("/c" "start")
      browse-url-browser-function 'browse-url-generic)


;; https://www.reddit.com/r/emacs/comments/6xryqh/emacs_in_wsl_and_the_windows_clipboard/
(defun wsl-copy (start end)
  "Copy region into Windows clipboard."
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (message "Copied: %s" (buffer-substring start end)))
