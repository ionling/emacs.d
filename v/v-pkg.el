;;; v-pkg.el --- Vision package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(require 'use-package)


(defmacro v-require (feature)
  "Like `require', but will download package when FEATURE not found."
  `(progn
     (unless (assoc ',feature package-alist)
       (use-package ,feature))
     (require ',feature)))


(v-require f)


(defun v-ensure-package (pkg)
  "Ensure PKG installed."
  (unless (assoc pkg package-alist)
    (let* ((filename (concat (symbol-name pkg) ".el"))
           (v-file (f-join user-emacs-directory "v" filename))
           (site-file (f-join user-emacs-directory "site-lisp" filename)))
      (cond ((f-exists-p v-file) (package-install-file v-file))
            ((f-exists-p site-file) (package-install-file site-file))))))


(provide 'v-pkg)

;;; v-pkg.el ends here
