;;; ja-ui.el --- UI configuration
;;; Commentary:
;;; Code:

(require 'dash)
(require 'use-package)

(require 'f)

(require 'aa-core)


;;;; Font
(defun v-font-set (name size)
  "Set font with NAME and SIZE."
  (let ((font (format "%s %d" name size)))
    (set-face-attribute 'default t :font font)
    (set-frame-font font nil t)))

;; REF https://emacs-china.org/t/topic/20587
;; REF https://stackoverflow.com/a/19547480

(defun v-font-set-cjk (name)
  "Set CJK font to NAME."
  (interactive)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family name))))

(defun v-font-find-by-name (name)
  "Find font by NAME."
  (find-font (font-spec :name name)))

;; JetBrains Mono
;; JetBrainsMono Nerd Font
;; Cascadia Code
;; 霞鹜文楷等宽

(defun v-font-set-meslo ()
  "Set font to MesloLGS."
  (interactive)
  (v-font-set "MesloLGS Nerd Font Mono" 12))

(add-hook 'v-ui-hook
          (lambda ()
            (if (equal (system-name) "ion-gnome")
                (v-font-set-meslo))))


;;;; Theme

(defgroup v-theme nil
  "Vision theme."
  :group 'v-ui
  :prefix "v-theme-")

(defcustom v-theme-always-enabled-list '(better-prog)
  "Always enabled themes when switching."
  :type '(repeat symbol)
  :group 'v-theme)

;;;; Theme
(defvar v-theme-last nil "Last theme.")
(defvar v-theme-last-file "~/.emacs.d/last-theme" "File which save last theme.")

;; REF https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/
(defun v-theme-switch (theme)
  "Disable active themes and load THEME."
  (interactive (->> (custom-available-themes)
                    (completing-read "Theme: ")
                    intern list))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (setq v-theme-last theme)
  (dolist (theme v-theme-always-enabled-list)
    (load-theme theme t))
  (message "[theme] Switched to %s" theme))

(defun v-theme-random ()
  "Switch to a random theme."
  (interactive)
  (let ((theme (seq-random-elt (custom-available-themes))))
    (v-theme-switch theme)))

(defun v-theme-store-file (theme)
  "Store THEME to file."
  (f-write (prin1-to-string theme) 'utf-8 v-theme-last-file))

(defun v-theme-load-file ()
  "Load theme from `v-theme-last-file'."
  (interactive)
  (let ((theme (intern (f-read v-theme-last-file 'utf-8))))
    (v-theme-switch theme)))

(advice-add 'v-theme-switch :after #'v-theme-store-file)


(use-package line-reminder
  :if window-system
  :hook (prog-mode . line-reminder-mode)
  :custom
  (line-reminder-show-option 'indicators)
  (line-reminder-thumbnail t))

(use-package minibuffer-line
  :defer 1
  :custom
  (minibuffer-line-refresh-interval 1)
  (minibuffer-line-format
   '("" (:eval system-name)
     " | " (:eval (format-time-string "%F %T"))
     " | " (:eval (if buffer-file-truename
                      buffer-file-truename
                    "no file name"))))
  :config
  (minibuffer-line-mode))

(use-package all-the-icons :disabled)

(use-package all-the-icons-ivy-rich
  :disabled
  :hook (v-ui . all-the-icons-ivy-rich-mode))

(use-package nerd-icons-ivy-rich
  :hook (v-ui . nerd-icons-ivy-rich-mode))

(use-package pangu-spacing
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package sideline
  :if window-system
  :delight
  :custom
  (sideline-display-backend-name t)
  (sideline-delay .4)
  :hook
  (prog-mode . sideline-mode))

;; https://i.loli.net/2021/02/18/6JTOmoUdvt5yf1w.gif
(use-package volatile-highlights
  :delight
  :hook (v-ui . volatile-highlights-mode))



(defun yes-doom-ui ()
  (interactive)

  (defun v-mode-line-toggle-encoding ()
    "Toggle the buffer encoding display."
    (interactive)
    (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding)))

  (use-package doom-themes
    :config
    (doom-themes-visual-bell-config)
    (with-eval-after-load 'org
      (doom-themes-org-config))
    (with-eval-after-load 'neotree
      (doom-themes-neotree-config))
    (with-eval-after-load 'treemacs
      (doom-themes-treemacs-config)))

  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-encoding nil)
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon t)
    (doom-modeline-minor-modes t)
    :hook (v-ui . doom-modeline-mode)))



(v-defmodule tabs
  (use-package centaur-tabs
    :disabled
    :defer 1
    :config
    ;; Fix bar is too high in mac.
    (if (not (eq window-system 'mac))
        (setq centaur-tabs-set-bar 'over))
    (centaur-tabs-mode)))


;; Random theme at each start
(v-with-idle-timer .1
  (v-theme-random))

(provide 'ja-ui)
;;; ja-ui.el ends here
