;;; init-editor.el --- Config of common editor features
;;; Commentary:
;;; Code:
(require 's)

(require 'init-core)


(defcustom v-file-save-place t
  "Automatically save place in each file and go to the place at next visiting."
  :type 'boolean
  :group 'v-file)


;;;; Better defaults
(set-default 'fill-column 100)
(setq column-number-indicator-zero-based nil)
(setq make-backup-files nil)
(setq selection-coding-system 'utf-8)

(if (eq window-system 'mac)
    (setq mac-option-modifier 'meta
          mac-command-modifier nil))


(defun v-default-editor ()
  "Editor related default configs."
  (setq-default tab-width 4)
  (column-number-mode)
  (global-auto-revert-mode)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (window-divider-mode 0)
  (setq indicate-buffer-boundaries t)
  ;; In macOS, disabling `menu-bar-mode' will make maximize button not working
  (unless (eq window-system 'mac)
    (menu-bar-mode 0)))

(add-hook 'v-editor-hook #'v-default-editor)


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
(use-package ibuffer :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package bufler)


;;;; Clipboard

(use-package browse-kill-ring)

(use-package xclip
  :hook (v-editor . xclip-mode))

(use-package undo-tree
  :delight
  :custom
  (undo-tree-auto-save-history nil)
  :hook (v-editor . global-undo-tree-mode))


;;;; Comment
(use-package comment-dwim-2
  :disabled
  :bind ("M-;" . comment-dwim-2))

(use-package banner-comment)

(use-package saveplace
  :if v-file-save-place
  :hook (v-editor . save-place-mode))


;;;; Display fill-column
(use-package display-fill-column-indicator
  :disabled                             ; It's not pretty
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
  (if is-wsl
      (setq server-socket-dir (v-join-user-emacsd "server"))))


;;;; Fill paragraph
(use-package aggressive-fill-paragraph
  :disabled                             ; In favor of semantic linefeeds
  :hook (org-mode . aggressive-fill-paragraph-mode))

;;;; Frame

(use-package ime-frame :v-ensure)

;;;; Indent
(set-default 'indent-tabs-mode nil)

(use-package aggressive-indent
  :hook (v-editor . global-aggressive-indent-mode))

(use-package indent-bars
  :hook ((python-base-mode go-mode) . indent-bars-mode))

;;;; Keyboard
(use-package keyfreq
  :defer 1
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;;;; Multiple
(use-package iedit
  :general
  (v-point-map "e" #'iedit-mode))


;;;; Outline
(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  :hook
  (prog-mode . outline-minor-mode))


;;;; Paren edit
(use-package smartparens
  :tags parens
  :delight
  :hook ((v-editor . smartparens-global-mode)
         ;; https://i.loli.net/2020/12/28/SIGCO29ZDX6wBYl.gif
         (v-editor . show-smartparens-global-mode))
  :config
  (require 'smartparens-config))        ; Load default config


;;;; Region
(use-package expand-region
  :tags region
  :bind ("C-=" . er/expand-region))


;;;; Scrolling
(use-package smooth-scrolling
  :hook (v-editor . smooth-scrolling-mode))


;;;; Search
(use-package deadgrep
  :tags search
  :exec rg
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

(defun v-thing-forward-string (&optional arg)
  "Move forward to ARGth string."
  (setq arg (or arg 1))
  (if (not (bobp))
      (save-match-data
        (when (or (and (looking-at-p "\\s-*\"")
                       (not (looking-back "\\\\")))
                  (re-search-backward "[^\\\\]\"" nil nil))
          (looking-at "\\s-*\"")
          (goto-char (match-end 0))
          (forward-char -1))))
  (while (and (> arg 0)
              (not (eobp))
              (looking-at-p "\\s-*\""))
    (forward-sexp 1)
    (setq arg (1- arg)))
  (while (and (< arg 0)
              (not (bobp))
              (looking-at-p "\""))
    (forward-sexp -1)
    (setq arg (1+ arg)))
  (ignore))

;; Tell the thingatpt.el library about it.
(put 'string 'forward-op 'v-thing-forward-string)

;; REF https://www.emacswiki.org/emacs/StringAtPoint

(defun v-thing-kill-string (&optional arg)
  "Kill ARG strings under point."
  (interactive "*p")
  (setq arg (or (and (not (zerop arg)) arg) 1))
  (if (> arg 0)
      (copy-region-as-kill
       (progn (forward-thing 'string 0) (point))
       (progn (forward-thing 'string arg) (point)))
    (copy-region-as-kill
     (progn (forward-thing 'string 1) (point))
     (progn (forward-thing 'string arg) (point)))))

;; REF https://www.emacswiki.org/emacs/IncrementNumber

(defun v-thing-increment-number ()
  "Increment the number at point."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun is-lower (char)
  "Check whether the CHAR is lowercase."
  (and (>= char 97) (<= char 122)))

(defun is-upper (char)
  "Check whether the CHAR is uppercase."
  (and (>= char 65) (<= char 90)))

;; Demos:
;; - URLString
;; - UrlString
;; - url_string

(defun v-thing-mark-dwim ()
  "Mark thing dwim."
  (interactive)
  (let (end)
    (pcase (char-after)
      ((pred is-lower)
       (while (is-lower (following-char))
         (forward-char))
       (setq end (point))
       (while (is-lower (preceding-char))
         (backward-char))
       (if (is-upper (preceding-char))
           (backward-char))
       ;; (setq begin (point))
       (push-mark end)
       (setq mark-active t))
      ((pred is-upper)
       (while (is-upper (following-char))
         (forward-char))
       (if (is-lower (following-char))
           (backward-char))
       (setq end (point))
       (while (is-upper (preceding-char))
         (backward-char))
       ;; (setq begin (point))
       (push-mark end)
       (setq mark-active t)))))

(defun v-thing-mark-line ()
  "Select current line."
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)))

;;;; Window

(defvar v-window-map (make-sparse-keymap))

(use-package popwin
  :hook (v-editor . popwin-mode)
  :config
  (delete 'help-mode popwin:special-display-config))

(use-package transpose-frame)

(use-package winner :ensure nil
  :doc "Restore old window configurations"
  :hook (v-editor . winner-mode))

(use-package golden-ratio
  :delight " GR"
  :doc
  "Enlarge the selected window"
  :custom
  (golden-ratio-adjust-factor 0.9))

(defalias #'v-window-golden #'golden-ratio)

(defun v-window-golden-right ()
  "Split window to right with golden ratio."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (golden-ratio t))


;;;; Whitespace

(use-package whitespace :ensure nil
  :delight global-whitespace-mode
  :custom
  (whitespace-line-column 100)
  (whitespace-style
   '(face trailing tabs lines-tail newline empty space-before-tab space-after-tab))
  :init
  (defun v-whitespace-org-mode-local-style ()
    (setq-local whitespace-style
                (remove 'lines-tail whitespace-style)))
  :hook
  (org-mode . v-whitespace-org-mode-local-style)
  (v-editor . global-whitespace-mode))

;; (setq-default show-trailing-whitespace t)


(use-package ws-butler
  :delight
  :doc "Unobtrusively remove trailing whitespace"
  :hook (v-editor . ws-butler-global-mode))


;;;; Misc

(use-package string-inflection)

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


(use-package mwim
  :tags move
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

;; REF http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/

(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun wc ()
  "Word count supporting Chinese, Japanese, and English.
「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string (buffer-substring-no-properties (point-min) (point-max)))
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
        (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" "" v-buffer-string)))
    (setq v-buffer-string ; 把註解行刪掉（不把註解算進字數）。
          (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數  （不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(provide 'init-editor)
;;; init-editor.el ends here
