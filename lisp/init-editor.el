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
  :defer 3
  :config (focus-autosave-mode))


;;;; Buffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


(use-package saveplace
  :if v-file-save-place
  :defer 2
  :config
  (save-place-mode))


;;;; Editor server
(use-package server
  :defer 3
  :config
  (if (not (or (daemonp) (server-running-p)))
      (server-start)))


;;;; Fill paragraph
(use-package aggressive-fill-paragraph
  :hook (org-mode . aggressive-fill-paragraph-mode))


;;;; Indent
(set-default 'indent-tabs-mode nil)

(use-package aggressive-indent
  :defer 3
  :config (global-aggressive-indent-mode))


;;;; Outline
(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  :hook
  (prog-mode . outline-minor-mode))


;;;; Paren edit
(use-package smartparens
  :delight
  :defer 3
  :config
  (require 'smartparens-config)         ; Load default config
  (smartparens-global-mode)
  ;; https://i.loli.net/2020/12/28/SIGCO29ZDX6wBYl.gif
  (show-smartparens-global-mode))


;;;; Region
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;;;; Scrolling
(use-package smooth-scrolling
  :defer 3
  :config (smooth-scrolling-mode))


;;;; Search
(use-package deadgrep
  :bind
  (:map v-point-map
        ("g" . deadgrep)))              ; Grep


;;;; Whitespace
(use-package ws-butler
  :delight
  :defer 3
  :config (ws-butler-global-mode))


(provide 'init-editor)
;;; init-editor.el ends here
