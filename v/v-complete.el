;;; v-complete.el --- vision completion config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration completion
;; Version: 20230804
;; Package-Requires: (company company-box company-tabnine company-try-hard)

;;; Commentary:

;;; Code:

(require 'company)
(require 'company-tabnine)


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
        (company-idle-delay .3)
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
        :config
        (global-company-mode))

      (use-package company-box
        :delight
        :hook (company-mode . company-box-mode))

      (use-package company-tabnine
        :hook
        (org-mode . v-disable-tabnine-local)
        :config
        (push 'company-tabnine company-backends))

      (use-package company-try-hard
        :bind ("C-'" . company-try-hard)))))


(defun v-disable-tabnine-local ()
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
