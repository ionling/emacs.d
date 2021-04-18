;;; v-file.el --- File related functions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dash)


;;;###autoload
(defun v-copy-file-name ()
  "Copy file name to clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Yet, no file name")
    (kill-new buffer-file-name)
    (message "Copied file name: %s" buffer-file-name)))


;;;###autoload
(defun v-show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (if buffer-file-name
      (message buffer-file-name)
    (message "Yet, no file name")))


;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;;###autoload
(defun v-rename-file-and-buffer (new-name)
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
         ;; (rename-buffer new-name)
         (set-visited-file-name new-name)
         (set-buffer-modified-p nil))))


;; https://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
;;;###autoload
(defun v-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (y-or-n-p (format "Delete file and buffer %s ? " filename)))
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))


;;;###autoload
(defun v-make-temp-file-and-buffer (extension)
  "Create a temporary file with EXTENSION and open it."
  (interactive (list (read-string "File extension: ")))
  (->> (concat "." extension)
    (make-temp-file "" nil)
    find-file))


(provide 'v-file)
;;; v-file.el ends here
