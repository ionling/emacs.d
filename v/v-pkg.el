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


(defmacro v-ensure-package (pkg)
  "Ensure PKG installed."
  `(unless (assoc ',pkg package-alist)
     (package-install-file
      ,(f-join user-emacs-directory "v" (concat (symbol-name pkg) ".el")))))


(provide 'v-pkg)

;;; v-pkg.el ends here
