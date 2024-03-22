;;; v-file.el --- File related functions  -*- lexical-binding: t -*-

;; Version: 20240322

;;; Commentary:
;;; Code:

(require 'subr-x)

(require 'dash)


;;;; Buffer

;;;###autoload
(defun v-file-buffer-move (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;;###autoload
(defun v-file-buffer-rename (new-name)
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
(defun v-file-buffer-delete ()
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
(defun v-file-buffer-make-temp (extension)
  "Create a temporary file with EXTENSION and open it."
  (interactive (list (read-string "File extension: ")))
  (->> (concat "." extension)
       (make-temp-file "" nil)
       find-file))


;;;; Sudo

(defun doom--sudo-file-path (file)
  "Generate sudo path for FILE."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))


;;;###autoload
(defun v-file-sudo-find (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (doom--sudo-file-path file)))


;;;###autoload
(defun v-file-sudo-this ()
  "Open the current file as root."
  (interactive)
  (find-file
   (doom--sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))


;;;; Misc

;;;###autoload
(defun v-file-copy-name ()
  "Copy file name to clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Yet, no file name")
    (kill-new buffer-file-name)
    (message "Copied file name: %s" buffer-file-name)))


;;;###autoload
(defun v-file-show-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (if buffer-file-name
      (message buffer-file-name)
    (message "Yet, no file name")))


(provide 'v-file)
;;; v-file.el ends here
