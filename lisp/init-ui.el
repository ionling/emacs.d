;;; init-ui.el --- UI configuration
;;; Commentary:
;;; Code:

(require 'dash)


(defgroup v-theme nil
  "Vision theme."
  :group 'v-ui
  :prefix "v-theme-")

(defcustom v-theme-always-enabled-list '(better-prog)
  "Always enabled themes when switching."
  :type '(repeat symbol)
  :group 'v-theme)

;;;; Theme
;; REF https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/
(defun v-theme-switch (theme)
  "Disable active themes and load THEME."
  (interactive (->> (custom-available-themes)
                    (completing-read "Theme: ")
                    intern list))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (dolist (theme v-theme-always-enabled-list)
    (load-theme theme t))
  (message "[theme] Switched to %s" theme))

(defun v-theme-random ()
  "Switch to a random theme."
  (interactive)
  (let ((theme (seq-random-elt (custom-available-themes))))
    (v-theme-switch theme)))


(use-package all-the-icons)


(use-package pangu-spacing
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package sideline
  :delight
  :custom
  (sideline-display-backend-name t)
  (sideline-delay .4)
  :hook
  (v-ui . global-sideline-mode))

;; https://i.loli.net/2021/02/18/6JTOmoUdvt5yf1w.gif
(use-package volatile-highlights
  :delight
  :hook (v-ui . volatile-highlights-mode))



(v-defmodule doom-ui
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
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon t)
    (doom-modeline-minor-modes t)
    :hook (v-ui . doom-modeline-mode)))



(v-defmodule tabs
  (use-package centaur-tabs
    :custom
    (centaur-tabs-close-button "✕")
    (centaur-tabs-set-bar 'left)
    :init (centaur-tabs-mode)))

;; Random theme at each start
(v-with-idle-timer .1
  (v-theme-random))

(provide 'init-ui)
;;; init-ui.el ends here
