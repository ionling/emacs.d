;;; v-utils.el --- Some useful utility functions
;;; Commentary:
;;; Code:

;;;; File

(defun edit-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))


(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (message file-name)
      (message "Yet, no file name"))))


(defun copy-file-name ()
  "Copy file name to clipboard."
  (interactive)
  (xclip-set-selection 'clipboard (buffer-file-name))
  (message "Copied file name"))


;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive (->> (or buffer-file-name  "")
                    file-name-nondirectory
                    (read-string "New name: " )
                    list))
  (cond ((not buffer-file-name)
         (message "Buffer '%s' is not visiting a file!" (buffer-name)))
        ((get-buffer new-name)
         (message "A buffer named '%s' already exists!" new-name))
        (t
         (rename-file buffer-file-name new-name)
         (rename-buffer new-name))))


;; https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))


;;;###autoload
(defun doom/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method)
                         ":" (file-remote-p file 'user)
                         "@" (file-remote-p file 'host)
                         "|sudo:root@" (file-remote-p file 'host)
                         ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))


;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (doom/sudo-find-file (file-truename buffer-file-name)))



;;;; Misc

(defun completing-require (feature)
  "Load FEATURE with completion."
  (interactive
   (->> (completing-read "Feature: " features)
        intern list))
  (require feature))


;; https://www.emacswiki.org/emacs/InsertingTodaysDate
(defun insert-current-date ()
  "Insert today's date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))


(defun open-in-code ()
  "Open current file in vscode."
  (interactive)
  (shell-command (concat "code " (buffer-file-name))))


(provide 'v-utils)
;;; v-utils.el ends here
