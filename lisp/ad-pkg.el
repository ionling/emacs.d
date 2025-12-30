;;; v-pkg.el --- Vision package management -*- lexical-binding: t; -*-
;; Version: 20241017
;;; Commentary:
;;; Code:

(require 'package)
(require 'use-package)

(require 'dash)
(require 'f)

(require 'aa-core)


(defmacro v-require (feature)
  "Like `require', but will download package when FEATURE not found."
  `(progn
     (unless (assoc ',feature package-alist)
       (use-package ,feature))
     (require ',feature)))

;;;###autoload
(defun v-completing-require (feature)
  "Load FEATURE with completion."
  (interactive
   (->> (completing-read "Feature: " features)
        intern list))
  (require feature))


;;;; v-ensure-package

(defun v-pkg-compare-version (d1 d2)
  "Compare the versions of two package descs D1 and D2."
  (> (car (package-desc-version d1))
     (car (package-desc-version d2))))

;;;###autoload
(defun v-pkg-refresh (pkg)
  "Install the latest version of the PKG and delete olds."
  (interactive
   (->> (completing-read "Feature: " features)
        intern list))
  (v-pkg-install pkg)
  (let ((descs (->> (alist-get pkg package-alist)
                    (-uniq)
                    (-sort #'v-pkg-compare-version)
                    (-drop 1))))
    (dolist (desc descs)
      (package-delete desc))))

(defun v-pkg-install (pkg)
  "Install a PKG using `package-install-file'."
  (let* ((sym-name (symbol-name pkg))
         (filename (concat sym-name ".el"))
         (paths `(("v" ,filename)
                  ("site-lisp" ,sym-name)
                  ("site-lisp" ,filename))))
    (cl-loop for path in paths
             for file = (apply #'v-join-user-emacsd path)
             if (file-exists-p file)
             return (package-install-file file))))

(defun v-pkg-ensure (pkg)
  "Ensure a PKG is installed."
  (unless (assoc pkg package-alist)
    (v-pkg-install pkg)))

(defalias 'v-ensure-package #'v-pkg-ensure)


;;;; Keywords

;;;;; doc

(add-to-list 'use-package-keywords :doc)

(defvar v-pkg-docs nil "All docs of `:doc' keyword.")

(defun use-package-normalize/:doc (_name _keyword args)
  "Normalize `:doc' ARGS."
  (car args))

(defun use-package-handler/:doc (name _keyword doc rest state)
  "Store DOC for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((setq v-pkg-docs (plist-put v-pkg-docs ',name ,doc))))))

;;;;; exec

(push-after :exec :pin 'use-package-keywords)

(defvar v-pkg-exec-plist nil "Package related executables.")

(defun use-package-normalize/:exec (_name _keyword args)
  "Normalize `:exec' ARGS."
  (car args))

;; The `executable-find' may slow down startup,
;; so we are better to store them in a list.

(defun use-package-handler/:exec (name _keyword exec rest state)
  "Store EXEC for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((setq v-pkg-exec-plist (plist-put v-pkg-exec-plist ',name ',exec))))))

(defun v-pkg-exec-check ()
  "Check executables of all package."
  (interactive)
  (let ((buffer (get-buffer-create "*v exec check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (cl-loop for (name execs) on v-pkg-exec-plist by #'cddr
               do
               (unless (listp execs)
                 (setq execs (list execs)))
               (insert "* Not found executables")
               (insert " " (format-time-string "%Y-%m-%d %H:%M:%S\n"))
               (dolist (exec execs)
                 (unless (executable-find (symbol-name exec))
                   (insert (format "- %s > %s\n" name exec))))))
    (switch-to-buffer-other-window buffer)
    (org-mode)))

;;;;; v-ensure

(defun use-package-normalize/:v-ensure (_name _keyword args)
  "Normalize `:v-ensure' ARGS."
  args)

(defun use-package-handler/:v-ensure (name _keyword _ rest state)
  "Handle `:v-ensure' for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (v-ensure-package name)
  (use-package-process-keywords name rest state))

(push-after :v-ensure :disabled 'use-package-keywords)

;;;;; module

(add-to-list 'use-package-keywords :module)

(defun use-package-normalize/:module (_name _keyword args)
  "Normalize function of `use-package' module keyword.
ARGS is arguments passed to this keyword."
  (car args))

(defun use-package-handler/:module (name _keyword module rest state)
  "Handle MODULE keyword for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (add-to-list 'v-modules module)
  (let ((body (use-package-process-keywords name rest state))
        (func (intern (format "v-mod-%s-load-%s" module name)))
        (var (intern (format "v-mod-%s-load-funcs" module)))
        (mod-var (intern (format "v-mod-group-%s" module))))
    `((defvar ,var nil
        ,(format "Load funcs of `%s'." mod-var))
      (defvar ,mod-var nil
        ,(format "The packages of `%s'." mod-var))
      (defun ,func ()
        ,(format "Load the `%s' package for `%s'." name mod-var)
        (interactive)
        ,@body)
      (add-to-list ',var #',func)
      (add-to-list ',mod-var ',name))))

;;;;; tags

(add-to-list 'use-package-keywords :tags)

(defvar v-pkg-tags nil "All package tags.")

(defun use-package-normalize/:tags (_name _keyword args)
  "Normalize ARGS for `:tags' keyword."
  args)

(defun use-package-handler/:tags (name _keyword args rest state)
  "Handle `:tags' ARGS for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (let ((body (use-package-process-keywords name rest state)))
    (apply #'use-package-concat
           body
           (mapcar
            (lambda (tag)
              (let ((tag-var (intern  (format "v-pkg-tag-%s" tag))))
                `((unless (boundp ',tag-var)
                    (defvar ,tag-var nil
                      ,(format "Tags for package %s." name)))
                  (add-to-list ',tag-var ',name )
                  (add-to-list 'v-pkg-tags ',tag))))
            args))))

;;;; Advices

(defvar v-pkg-all '(use-package))

(defun v-pkg-use-package-after-advice (name &rest _args)
  "Advice for `use-package' macro, Add NAME to `v-pkg--all-packages'."
  (push name v-pkg-all))

(advice-add 'use-package :after #'v-pkg-use-package-after-advice)

;;;###autoload
(defun v-pkg-report ()
  "Show all packages."
  (interactive)
  (let ((buffer (get-buffer-create "*v packages*")))
    (with-current-buffer buffer
      (dolist (pkg (reverse v-pkg-all))
        (insert (format "- %s\n" pkg))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun v-pkg-show-version (pkg)
  "Show the highest version of a PKG."
  (interactive
   (->> (-map #'car package-alist)
        (completing-read "Package: ")
        list))
  (->> (f-glob (concat pkg "*") (v-join-user-emacsd "elpa"))
       (-map #'f-filename)
       (-sort #'string-greaterp)
       -first-item
       message))

(provide 'ad-pkg)

;;; ad-pkg.el ends here