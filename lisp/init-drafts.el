;;; init-drafts.el --- Some drafts here  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-core)
(require 'use-package)

(require 's)


;; Started at <2023-03-04 Sat>
(use-package auto-package-update
  :disabled
  :defer 1
  :config
  (auto-package-update-maybe))

(when nil
  (setq doom-modeline-icon nil)

  ;; Not working in WSL2 Windows Terminal
  (use-package evil-terminal-cursor-changer :disabled)

  ;; Not working with TMUX and Alacritty
  (use-package clipetty :disabled))

(defun v-company-space ()
  "Call `company-abort' and insert a space."
  (interactive)
  (company-abort)
  (insert " "))

(when nil
  (with-eval-after-load 'company
    (general-def company-active-map "SPC" #'v-company-space)))
;;;; Complete v2
(when nil
  (use-package vertico
    :hook v-complete-v2)

  (use-package orderless
    :ensure t
    :init
    (push 'orderless completion-styles)
    :custom
    (completion-category-overrides '((file (styles basic partial-completion)))))

  ;; Enable rich annotations using the Marginalia package
  (use-package marginalia
    ;; Bind `marginalia-cycle' locally in the minibuffer.
    ;; To make the binding available in the *Completions* buffer,
    ;; add it to the `completion-list-mode-map'.
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))

    ;; The :init section is always executed.
    :init

    ;; Marginalia must be activated in the :init section of use-package
    ;; such that the mode gets enabled right away.
    ;; Note that this forces loading the package.
    (marginalia-mode))

  ;; https://emacs-china.org/t/straight-ivy-helm-selectrum/11523/113

  (use-package corfu
    ;; Optional customizations
    :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `global-corfu-modes'.
    :init
    (global-corfu-mode))

  (v-with-idle-timer 1
    (run-hooks 'v-complete-v2-hook)))


;;;; Comments

;; (use-package graph :ensure nil
;;   :quelpa (graph :fetcher github :repo storax/graph.el))
;; (use-package litable)
;; (use-package dired-sidebar)
;;     symbols-outline
;;     do-at-point
;;     async-status
;;     expreg
;;     subword-mode


;;;; Disabled

(use-package dtrt-indent :disabled)

(use-package el-docstring-sap
  :disabled
  :quelpa (el-docstring-sap :fetcher github :repo rileyrg/el-docstring-sap)
  :hook
  (emacs-lisp-mode . el-docstring-sap-mode))

;; 2021-02-01
(use-package org-appear :ensure nil
  :disabled
  :quelpa (org-appear :fetcher github :repo awth13/org-appear))

(use-package org-protocol-capture-html
  :disabled
  :ensure nil
  :quelpa (org-protocol-capture-html :fetcher github :repo "alphapapa/org-protocol-capture-html"))

(use-package rotate-text :ensure nil
  :disabled
  :quelpa (rotate-text :fetcher github :repo nschum/rotate-text.el)
  :config
  ;; `rotate-text-words' will auto convert capitalized word,
  ;; e.g., True -> False, TRUE -> FALSE,
  ;; and `rotate-text-symbol' will not.
  (add-to-list 'rotate-text-words '("true" "false")))

(use-package smart-jump :disabled)

(use-package visual-fill-column
  :disabled
  :custom
  (fill-column 100)
  :config
  (global-visual-fill-column-mode))

;;;; Misc

(defun v-golang-paypal-gen-enum (name)
  "Convert PayPal enum NAME to Golang code."
  (interactive "sName:")
  (->> name
       s-trim
       (s-split "\\.")
       (-map #'s-capitalize)
       (s-join "")
       (format (concat "	%s EventType = \"" name "\""))
       insert))

(provide 'init-drafts)
;;; init-drafts.el ends here
