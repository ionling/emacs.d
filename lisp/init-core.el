;;; init-core.el --- Config core
;;; Commentary:
;;; Code:
(defvar v-modules nil)

(defvar v-modules-loaded nil)


(defmacro v-defmodule (name &rest body)
  "Define a NAME module.  BODY are forms to eval."
  (declare (indent defun))
  (if v-modules
      (add-to-list 'v-modules name)
    (setq v-modules (list name)))
  (let ((func-name (format "v-load-%s" (symbol-name name))))
    `(defun ,(intern func-name) ()
       ,(format "Load the module %s" name)
       (interactive)
       ,@body)))


(defmacro v-load (&rest modules)
  "Load MODULES."
  (dolist (module modules)
    (cond ((not (memq module v-modules))
           (user-error "No module: %s" module))
          ((memq module v-modules-loaded) "Loaded")
          (t
           (if v-modules
               (add-to-list 'v-modules-loaded module)
             (setq v-modules (list module)))
           (funcall (intern (format "v-load-%s" (symbol-name module))))))))


(defmacro v-defun (name &optional docstring)
  "Define a function which is configurable via variable.
NAME specified function name, DOCSTRING as well."
  (let* ((func-name (format "v-%s" (symbol-name name)))
         (func-var (format "%s-func" func-name))
         (func-var-symbol (intern func-var)))
    `(progn
       (defvar ,func-var-symbol
         nil ,(format "Function that `%s' uses." func-name))

       (defun ,(intern func-name) ()
         ,(format "%s\nSee `%s'." docstring func-var)
         (interactive)
         (funcall ,func-var-symbol)))))



;;;; Package
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;; benchmark-init
(if (not (package-installed-p 'benchmark-init))
    (progn
      (package-refresh-contents)
      (package-install 'benchmark-init)))

(require 'benchmark-init)
(benchmark-init/activate)
(add-hook 'after-init-hook #'benchmark-init/deactivate)

;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t)


(use-package quelpa
  :custom
  (quelpa-checkout-melpa-p nil))


(use-package quelpa-use-package :demand)

(use-package gnu-elpa-keyring-update)

(use-package delight)



;;;; Key binding
(defvar vision-map (make-sparse-keymap)
  "Personal keymap.")

(defvar v-point-map (make-sparse-keymap))

(use-package general :demand)

(general-def vision-map "t" v-point-map)


(use-package which-key
  :delight
  :general
  (vision-map
   :prefix "k"
   :prefix-command 'v-key-map
   "k" #'which-key-show-keymap
   "m" #'which-key-show-major-mode
   "t" #'which-key-show-top-level)
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-right-bottom))



;;;; Project
(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  :general
  (vision-map "p" projectile-command-map)
  :init
  (projectile-mode)
  (setq projectile-mode-line-function
        (lambda ()
          (format " P:%s"
                  (if (< (length projectile-project-name) 5)
                      (projectile-project-name)
                    (concat (substring projectile-project-name 0 4) "..")))))
  :config
  (projectile-mode))


(provide 'init-core)
;;; init-core.el ends here
