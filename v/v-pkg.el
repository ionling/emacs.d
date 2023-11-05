;;; v-pkg.el --- Vision package management -*- lexical-binding: t; -*-
;; Version: 20231105
;;; Commentary:
;;; Code:

(require 'package)

(require 'use-package)

(require 'init-core)


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
(defun v-ensure-package (pkg)
  "Ensure PKG installed."
  (unless (assoc pkg package-alist)
    (let* ((sym-name (symbol-name pkg))
           (filename (concat sym-name ".el"))
           (paths `(("v" ,filename)
                    ("site-lisp" ,sym-name)
                    ("site-lisp" ,filename))))
      (cl-loop for path in paths
               for file = (apply #'v-join-user-emacsd path)
               if (file-exists-p file)
               return (package-install-file file)))))


;;;;; use-package keyword
(defun use-package-normalize/:v-ensure (_name _keyword args)
  "Normalize `:v-ensure' ARGS."
  args)

(defun use-package-handler/:v-ensure (name _keyword _ rest state)
  "Handle `:v-ensure' for NAME package.
see `use-package-process-keywords' for REST and STATE."
  (v-ensure-package name)
  (use-package-process-keywords name rest state))

(push-after :v-ensure :disabled 'use-package-keywords)

;;;;;; tags
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

(provide 'v-pkg)

;;; v-pkg.el ends here
