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



(v-defmodule auto-theme

  ;; TODO defcustom
  (setq v-light-theme 'doom-one-light)
  (setq v-dark-theme 'doom-one)

  (use-package solar :ensure nil
    :custom
    ;; Get them at https://www.latlong.net
    (calendar-latitude 30.572815)
    (calendar-longitude 104.066803))

  (use-package circadian
    :custom
    (circadian-themes `((:sunrise . ,v-light-theme)
                        (:sunset  . ,v-dark-theme)))
    :hook (emacs-startup . circadian-setup)))


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


(v-defmodule highlight
  (use-package highlight-numbers
    :hook (prog-mode . highlight-numbers-mode))


  (use-package highlight-parentheses :delight
    :defer 6
    :config (global-highlight-parentheses-mode))


  (use-package hl-line
    :defer 6
    :config (global-hl-line-mode))


  (use-package hl-todo
    :defer 6
    :config
    (add-to-list 'hl-todo-keyword-faces '("REF" . "#20abb0"))
    (global-hl-todo-mode))


  (use-package beacon
    :delight
    :defer 6
    :config
    (beacon-mode))


  (use-package color-identifiers-mode
    :delight
    :init (global-color-identifiers-mode))


  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))


  (use-package symbol-overlay
    :defer 2
    :delight
    :general
    (symbol-overlay-map "c" #'symbol-overlay-count)
    :config
    (general-def 'vision-map "s" symbol-overlay-map)
    (symbol-overlay-mode)))


(v-defmodule tabs
  (use-package centaur-tabs
    :custom
    (centaur-tabs-close-button "✕")
    (centaur-tabs-set-bar 'left)
    :init (centaur-tabs-mode)))


(provide 'init-ui)
;;; init-ui.el ends here
