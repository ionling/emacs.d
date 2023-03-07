;;; init-editor.el --- Config of common editor features
;;; Commentary:
;;; Code:
(require 's)
(require 'outline)

(require 'init-core)


(defcustom v-file-save-place t
  "Automatically save place in each file and go to the place at next visiting."
  :type 'boolean
  :group 'v-file)


;;;; Better defaults
(set-default 'fill-column 80)
(setq column-number-indicator-zero-based nil)
(setq make-backup-files nil)


(defun buffer-name-list ()
  "Get all buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun window-count ()
  "Get window count of the selected frame."
  (length (window-list)))


(defun v-outline-hide-body-other-window ()
  "Call `outline-hide-body' in other indirect buffer window."
  (interactive)
  (let* ((cur-buf (current-buffer))
         (buf-name (buffer-name cur-buf))
         ;; Get max indirect buffer ID of current base buffer
         (max-id (->> (buffer-name-list)
                      (-filter (lambda (x) (s-starts-with? buf-name x) ))
                      (-map (lambda (x) (s-replace buf-name "" x)))
                      (-map #'string-to-number)
                      (-map #'-)
                      (-sort #'>)
                      (-first-item)))
         (new-name (format "%s-%d" buf-name (1+ max-id))))
    (make-indirect-buffer cur-buf new-name t)
    (if (eq (window-count) 1)
        (split-window-right))
    (other-window 1)
    (switch-to-buffer new-name)
    (outline-hide-body)))


;;;; Autosave
(use-package focus-autosave-mode
  :delight
  :hook (v-editor . focus-autosave-mode))


;;;; Buffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


(use-package saveplace
  :if v-file-save-place
  :hook (v-editor . save-place-mode))


;;;; Display fill-column
(use-package display-fill-column-indicator
  :hook (v-editor . global-display-fill-column-indicator-mode))


;;;; Editor server
(defun v-server-start ()
  "Call `server-start' if no server is running."
  (require 'server)
  (if (not (or (daemonp) (server-running-p)))
      (server-start)))

(use-package server
  :hook (v-editor . v-server-start)
  :config
  ;; fix: Getting attributes: Permission denied, /mnt/wslg/runtime-dir/emacs
  (setq server-socket-dir (v-join-user-emacsd "server")))


;;;; Fill paragraph
(use-package aggressive-fill-paragraph
  :hook (org-mode . aggressive-fill-paragraph-mode))


;;;; Indent
(set-default 'indent-tabs-mode nil)

(use-package aggressive-indent
  :hook (v-editor . global-aggressive-indent-mode))


;;;; Outline
(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  :hook
  (prog-mode . outline-minor-mode))


;;;; Paren edit
(use-package smartparens
  :delight
  :hook ((v-editor . smartparens-global-mode)
         ;; https://i.loli.net/2020/12/28/SIGCO29ZDX6wBYl.gif
         (v-editor . show-smartparens-global-mode))
  :config
  (require 'smartparens-config))        ; Load default config


;;;; Region
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;;;; Scrolling
(use-package smooth-scrolling
  :hook (v-editor . smooth-scrolling-mode))


;;;; Search
(use-package deadgrep
  :bind
  (:map v-point-map
        ("g" . deadgrep)))              ; Grep


;;;; Thing at point
(use-package thing-edit
  :v-ensure
  :general
  (v-point-map
   "n" #'thing-copy-number
   "s" #'thing-copy-symbol
   "u" #'thing-copy-url
   "w" #'thing-copy-word))


;;;; Whitespace
(use-package ws-butler
  :delight
  :hook (v-editor . ws-butler-global-mode))


(defun v-copy-and-comment-region (beg end &optional arg)
  "Duplicate the region BEG to END and comment out the copied text.
See `comment-region' for prefix ARG.
This is useful when you want to refactor some code.
Refer https://stackoverflow.com/a/23588908/7134763."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg))


(provide 'init-editor)
;;; init-editor.el ends here
