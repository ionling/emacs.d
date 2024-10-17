;;; init-core.el --- Config core
;;; Commentary:
;;; Code:
(require 'package)


(defvar v-modules nil)

(defvar v-modules-loaded nil)

(defvar is-wsl (length> (getenv "WSL_DISTRO_NAME") 0)
  "Whether in WSL environment.")


;;;; Callable

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


(defmacro v-with-idle-timer (secs &rest body)
  "Perform BODY the next time Emacs is idle for SECS seconds.
A wrapper of `run-with-idle-timer'."
  (declare (indent 1))
  `(run-with-idle-timer
    ,secs nil
    (lambda ()
      ,@body)))


(defmacro v-defmodule (name &rest body)
  "Define a NAME module.  BODY are forms to eval."
  (declare (indent defun))
  (add-to-list 'v-modules name)
  (let ((func-name (format "v-load-%s" (symbol-name name))))
    `(defun ,(intern func-name) ()
       ,(format "Load the module %s" name)
       (interactive)
       ,@body)))


(defmacro v-load (&rest modules)
  "Load MODULES."
  `(dolist (module ',modules)
     (cond ((not (memq module v-modules))
            (user-error "No module: %s" module))
           ((memq module v-modules-loaded) "Loaded")
           (t
            (funcall (intern (format "v-load-%s" (symbol-name module))))
            (add-to-list 'v-modules-loaded module)))))


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
           (v-ensure-package ',(intern (format "v-%s" module)))
           (funcall #',(intern (format "v-%s-config" module)))
           (setq ,symbol t)))
       (with-eval-after-load ',package
         (advice-add #',module-mode :before #',symbol)))))


(defvar v-init-by-file-ext nil "Plist of file extension to init function.")

(defun v-init-register-ext (ext func)
  "Register the file EXT with the FUNC."
  (setq v-init-by-file-ext (plist-put v-init-by-file-ext ext func)))

;; Don't use f-ext, as we want to minimize dependencies
;; when Emacs is first booted without ELPA packages.
(defun v-init-by-file-ext ()
  "Automatically run the init function according to file extension."
  (let* ((ext (file-name-extension buffer-file-name))
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


(defun v-dep-hook-symbal (name)
  "Return the symbal used by NAME `v-dep'."
  (intern (format "v-dep-%s-hook" name)))

(defmacro v-dep (name &rest args)
  "Declare a NAME block defined in ARGS.
Keywords:
:after         After another `v-dep' block
:idle          Like `run-with-idle-timer'
:do            Body to execute"
  (let ((after (plist-get args :after))
        (idle (plist-get args :idle))
        (do-func (intern (format "v-dep-do-%s" name)))
        finded do)
    (dolist (e args do)
      (if (and (keywordp e) finded)
          (setq finded nil))
      (if finded
          (push e do))
      (if (eq e :do)
          (setq finded t)))

    (when (eq do nil)
      (lwarn 'v-dep :warning "no do keyword of %s" name))

    (push `(run-hooks ',(v-dep-hook-symbal name)) do)
    (setq do (reverse do))

    `(progn
       (defun ,do-func ()
         ,@do)
       ,(if after
            `(add-hook ,(v-dep-hook-symbal after) ,do-func)
          (if idle
              `(v-with-idle-timer ,idle
                 (,do-func))
            `(,do-func))))))



;;;; Package

;; 2023-05-07 tsinghua:
;;   我们检测到您所在的子网和/或所使用的客户端存在大量下载某些较大二进制文件的行为，
;;   为保证用户的正常使用，我们阻断了此类请求。

(let* ((mirror-163 "https://mirrors.163.com")
       (mirror-tencent "https://mirrors.cloud.tencent.com")
       (mirror-tsinghua "https://mirrors.tuna.tsinghua.edu.cn")
       (mirror-ustc "https://mirrors.ustc.edu.cn")
       (mirror-zilongshanren "https://elpa.zilongshanren.com")
       (mirror mirror-ustc))
  (fset 'gen-mirror-url
        (lambda (name)
          `(,name . ,(format "%s/elpa/%s/" mirror name))))
  (setq package-archives
        `(,(gen-mirror-url "gnu")
          ,(gen-mirror-url "melpa")
          ,(gen-mirror-url "nongnu"))))


(setq custom-file (v-join-user-emacsd "custom.el"))
(v-with-idle-timer .08
  (when (file-exists-p custom-file)
    (load-file custom-file)))


;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package dash)

(use-package quelpa
  :disabled                             ; 2022-09-04 May slow Emacs startup
  :custom
  (quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package
  :disabled                             ; Same as up
  :demand)

(use-package gnu-elpa-keyring-update)

(use-package delight)


;;;; Key binding

(use-package general)
(require 'general)

(defvar vision-map (make-sparse-keymap)
  "Personal keymap.")

(defvar v-point-map (make-sparse-keymap))

(general-define-key :states 'motion "SPC" vision-map)
(general-def "C-c" vision-map)
(general-def vision-map "t" v-point-map)

;; (general-define-key :keymaps 'which-key-mode-map "i" nil)

(use-package use-package-chords
  :disabled
  :demand t
  :init
  (key-chord-mode 1))

(use-package which-key
  :delight
  :defer 2
  :custom
  (which-key-idle-delay 0.6)
  :general
  (vision-map
   :prefix "k"
   :prefix-command 'v-key-map
   "k" #'which-key-show-keymap
   "m" #'which-key-show-major-mode
   "t" #'which-key-show-top-level)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))



;;;; Project

(defun v-project-mode-line ()
  "Generate project mode line."
  (let ((name (projectile-project-name)))
    ;; 🧊 🧪 🌇 🚀 🍻 🧱 🦔 🎇 📚
    (format " 📚 %s"
            (if (< (length name) 7)
                name
              (format "%s.." (substring name 0 6))))))

(use-package projectile
  :defer 1
  :custom
  (projectile-completion-system 'ivy)
  (projectile-mode-line-function #'v-project-mode-line)
  (projectile-project-root-functions
   '(projectile-root-local
     projectile-root-top-down
     projectile-root-bottom-up
     projectile-root-top-down-recurring))
  :config
  (add-to-list 'projectile-project-root-files "go.mod")
  (general-def vision-map "p" projectile-command-map)
  (projectile-mode))


(provide 'init-core)
;;; init-core.el ends here
