;;; init-ui.el --- UI configuration
;;; Commentary:
;;; Code:

(require 'dash)

;;;; Theme
;; REF https://www.reddit.com/r/emacs/comments/ezetx0/doomthemes_screenshots_updated_good_time_to_go/
(defun ap/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (->> (custom-available-themes)
                    (completing-read "Theme: ")
                    intern list))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))


(use-package all-the-icons)


(use-package pangu-spacing
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))


;; https://i.loli.net/2021/02/18/6JTOmoUdvt5yf1w.gif
(use-package volatile-highlights
  :delight
  :defer 4
  :config (volatile-highlights-mode))



(v-defmodule doom-ui
  (use-package doom-themes
    :config
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-icon t)
    (doom-modeline-minor-modes t)
    :hook (after-init . doom-modeline-mode)))



(v-defmodule tabs
  (use-package centaur-tabs
    :custom
    (centaur-tabs-close-button "✕")
    (centaur-tabs-set-bar 'left)
    :init (centaur-tabs-mode)))


(provide 'init-ui)
;;; init-ui.el ends here
