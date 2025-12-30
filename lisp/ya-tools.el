;;; ya-tools.el --- Load some useful tools
;;; Commentary:
;;; Code:
(require 'use-package)

(require 'aa-core)
(require 'modules)


(use-package bug-hunter)

(use-package dashboard
  :defer 0.5
  :custom
  (dashboard-banner-logo-title "Be who you are")
  (dashboard-center-content  t)
  (dashboard-items '((projects . 5)
                     (recents  . 8)
                     (bookmarks . 4)
                     (registers . 4)))
  :config
  (dashboard-open))


(use-package google-this
  :general
  (v-point-map "h" #'google-this))


(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (("C-h f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h y" . helpful-at-point)))


;;;; Link

(use-package link-hint
  :general
  (v-point-map "c" #'link-hint-copy-link
               "o" #'link-hint-open-link))


(use-package wakatime-mode
  :exec wakatime
  :if (or (executable-find "wakatime-cli")
          (and (warn "wakatime cli not found") nil))
  :delight
  :defer 1
  :config (global-wakatime-mode))

;;;; English

(use-package youdao-dictionary
  :init
  (defun v-youdao-dict-search-at-point ()
    "Automatically call best `youdao-dictionary-search-at-point-' function."
    (interactive)
    (cond ((and (require 'posframe nil t) (posframe-workable-p))
           ;; Copied from `youdao-dictionary--posframe-tip'
           (youdao-dictionary-search-at-point-posframe))
          ((display-graphic-p) (youdao-dictionary-search-at-point-tooltip))
          (t (youdao-dictionary-search-at-point+))))
  :general
  (v-point-map "y" #'v-youdao-dict-search-at-point))

(use-package go-translate
  :custom
  (gt-langs '(en zh))
  :general
  (v-point-map "t" #'gt-do-translate)
  :config
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'word)
         :engines (list (gt-youdao-dict-engine))
         ;; :engines (list (gt-youdao-dict-engine) (gt-bing-engine) (gt-google-engine))
         :render (gt-buffer-render))))

(use-package reverso)

(use-package wordreference
  :custom
  (wordreference-source-lang "zh"))


;;;; Misc

(use-package paradox
  :defer 2
  :config
  (paradox-enable))

(use-package print-debug :v-ensure
  :config
  ;; REF https://emacs.stackexchange.com/a/3415
  (setcdr (assq 'go print-debug-ext-template) "fmt.Println(\"+++ %S\", )"))

(use-package restart-emacs)

(use-package try)


(use-package flyspell
  :delight " Fs"
  :if v-mod-spell-checking-enabled
  :hook
  (prog-mode . flyspell-prog-mode)
  (org-mode . flyspell-mode)
  :config
  (setq ispell-dictionary "en"))


(v-defmodule hydra
  (use-package hydra)

  (use-package hydra-posframe
    :disabled
    :ensure nil
    :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")
    :defer 2
    :custom
    (hydra-posframe-border-width 2)
    :custom-face
    (hydra-posframe-border-face ((t (:background "dark grey"))))
    :config
    (hydra-posframe-enable))

  (defhydra hydra-window (global-map "C-c w")
    "
^  ^Size^         ^^Switch^       ^Split^            ^Buffer      | Window |
^--^----^---------^^------^-------^-----^------------^------------+--------+
^    _k_          ^_n_ext         _v_ertical         _b_ switch
^    ^↑^          ^_p_revious     _x_ horizontal     _c_ close(kill)
_h_ ←   → _l_      _s_elect       _g_ golden         _r_ recentf
^    ^↓^          ^_d_elete
^    _j_
"
    ("h" (lambda ()
           (interactive)
           (shrink-window-horizontally 10)))
    ("l" (lambda ()
           (interactive)
           (enlarge-window-horizontally 10)))
    ("j" (lambda ()
           (interactive)
           (shrink-window 4)))
    ("k" (lambda ()
           (interactive)
           (enlarge-window 4)))
    ("n" other-window)
    ("p" (lambda ()
           (interactive)
           (other-window -1)))
    ("d" delete-window)
    ("s" ace-select-window)

    ("v" split-window-right)
    ("x" split-window-below)
    ("g" v-window-golden)

    ("b" ivy-switch-buffer)
    ("c" kill-current-buffer)
    ("r" counsel-recentf))

  ;; https://github.com/abo-abo/hydra/wiki/Org-clock-and-timers
  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("?" (org-info "Clocking commands"))))


(provide 'ya-tools)
;;; ya-tools.el ends here
