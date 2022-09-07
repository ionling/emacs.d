;;; init-core.el --- Config core
;;; Commentary:
;;; Code:
(require 'v-pkg)


(defvar v-modules nil)

(defvar v-modules-loaded nil)


;; Don't use `f' lib, as it will slow down Emacs startup (it requires many packages)
(defun v-join-user-emacsd (&rest paths)
  "Join PATHS with `user-emacs-directory'."
  (let ((res user-emacs-directory))
    (dolist (path paths res)
      (setq res (expand-file-name path res)))))


;; We can also use `cl-loop'.
;; See https://github.com/noctuid/general.el/blob/9651024e7f40a8ac5c3f31f8675d3ebe2b667344/general.el#L2578.
(defun push-after (elem pos list-sym)
  "Push ELEM after POS in LIST-SYM.
If ELEM not found, do nothing."
  (let (res)
    (dolist (i (symbol-value list-sym) res)
      (push i res)
      (if (eq i pos)
          (push elem res)))
    (set list-sym (reverse res))))


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


;; https://www.emacswiki.org/emacs/KeywordArguments
(defmacro v-init (module &rest args)
  "Automatically initialize MODULE.
Supported keyword ARGS:
:package        Package which define MODULE-mode, default is MODULE-mode."
  (let* ((symbol (intern (format "v-init-%s" module)))
         (module-mode (intern (format "%s-mode" module)))
         (package (or (plist-get args :package) module-mode)))
    `(progn
       (defvar ,symbol nil
         ,(format "Whether %s module initialized." module))
       (defun ,symbol ()
         ,(format "Initialize %s module." module)
         (unless ,symbol
           (v-ensure-package ,(intern (format "v-%s" module)))
           (funcall #',(intern (format "v-%s-config" module)))
           (setq ,symbol t)))
       (with-eval-after-load ',package
         (advice-add ',module-mode :before #',symbol)))))


(defvar v-init-by-file-ext nil "Plist of file extension to init function.")

(defun v-init-register-ext (ext func)
  "Register the file EXT with the FUNC."
  (setq v-init-by-file-ext (plist-put v-init-by-file-ext ext func)))

(defun v-init-by-file-ext ()
  "Automatically run the init function according to file extension."
  (let* ((ext (f-ext buffer-file-name))
         (inited (intern (format "v-init-%s" ext))))
    (when (and ext (not (boundp inited)))
      (let ((func (plist-get v-init-by-file-ext (intern ext))))
        (when func
          (funcall func)
          (set inited t))))))

(add-hook 'find-file-hook #'v-init-by-file-ext)


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
         (cond ((eq ,func-var-symbol nil)
                (message "%s not defined" ,func-var))
               ((commandp ,func-var-symbol)
                (call-interactively ,func-var-symbol))
               (t
                (funcall ,func-var-symbol)))))))



;;;; Package
(require 'package)

(let* ((mirror-163 "https://mirrors.163.com")
       (mirror-tencent "https://mirrors.cloud.tencent.com")
       (mirror-tsinghua "https://mirrors.tuna.tsinghua.edu.cn")
       (mirror mirror-tsinghua))
  (fset 'gen-mirror-url
        (lambda (name)
          `(,name . ,(format "%s/elpa/%s/" mirror name))))
  (setq package-archives
        `(,(gen-mirror-url "gnu")
          ,(gen-mirror-url "melpa")
          ,(gen-mirror-url "org"))))


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
