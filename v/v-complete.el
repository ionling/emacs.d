;;; v-complete.el --- vision completion config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration completion
;; Version: 20241012
;; Package-Requires: (company company-box)

;;; Commentary:

;;; Code:

(require 'v-pkg)

(v-require company)
(v-require company-tabnine)


;;;###autoload
(defun v-complete-config ()
  "Load python config."
  (interactive)
  (eval
   '(progn
      (use-package company
        :defer 4
        :custom
        (company-dabbrev-downcase nil)
        (company-idle-delay .2)
        (company-minimum-prefix-length 2)
        (company-show-numbers t)
        (company-tooltip-align-annotations t)
        (company-backends
         ;; The company-yasnippet backend will always shadow backends that come after it.
         '((company-capf :with company-yasnippet)
           company-files
           company-dabbrev-code
           company-keywords
           company-dabbrev))
        :general
        ("C-'" #'company-other-backend)
        :config
        (global-company-mode))

      (use-package company-box
        :delight
        :hook (company-mode . company-box-mode))

      ;; FIXME LSP mode 补全有问题, 需要关开一次 fuzzy-mode, 才能正常工作
      (use-package company-fuzzy
        :disabled
        :defer 3
        :config
        (global-company-fuzzy-mode))

      ;; According to its GitHub README:
      ;;   It has the following feature:
      ;;     1. It is fast enough for daily use.
      ;;     2. It works well with CJK language.
      ;;
      ;; But the first point, I don not care, and the second point,
      ;; I can not find CJK problems when using the origin UI, so I disabled it.
      (use-package company-posframe
        :disabled
        :defer 4
        :delight
        :hook (company-mode . company-posframe-mode))

      (use-package company-tabnine
        :disabled
        :hook
        ((org-mode . v-disable-tabnine-local)
         (prog-mode . v-tabnine-enable-local)))

      (use-package company-quickhelp
        :disabled
        :hook (company-mode . company-quickhelp-mode))

      (use-package company-statistics
        :disabled
        :hook (company-mode . company-statistics-mode)))))


(defun v-tabnine-enable-local ()
  "Enable Tabnine for the current local buffer."
  (interactive)
  (let ((clean-backends
         (remove 'company-tabnine company-backends)))
    (setq-local company-backends
                (push 'company-tabnine clean-backends))))

(defun v-tabnine-disable-local ()
  "Disable(not quit) tabnine in current buffer."
  (interactive)
  (setq-local company-backends
              (remove 'company-tabnine company-backends)))


(defun v-tabnine-quit ()
  "Remove `company-tabnine' backend and kill TabNine process."
  (interactive)
  (setq company-backends
        (delete 'company-tabnine company-backends))
  (company-tabnine-kill-process))


(defun v-tabnine-toggle ()
  "Toggle `company-tabnine' in `company-backends'."
  (interactive)
  (if (memq 'company-tabnine company-backends)
      (progn
        (setq company-backends
              (delete 'company-tabnine company-backends))
        (message "Disabled `company-tabnine`"))
    (push 'company-tabnine company-backends)
    (message "Enabled `company-tabnine`")))


(provide 'v-complete)
;;; v-complete.el ends here
