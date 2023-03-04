(require 'dash)
(require 'cl-macs)
(require 'f)


(defun doom-keyword-name (keyword)
  "Return the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))


(defun load-module (category module)
  "Load the MODULE of CATEGORY."
  (let* ((cat-name (doom-keyword-name category))
         (mod-dir (-> (format "modules/%s/%s" cat-name module)
                      (expand-file-name user-emacs-directory)))
         (mod-file (format "%s.el" mod-dir)))
    (cond ((f-directory? mod-dir)
           (load-file (f-join mod-dir "config.el")))
          ((f-file? mod-file)
           (load-file mod-file))
          (t (user-error "No module: %s/%s" category module)))))


(defmacro bootstrap (&rest modules)
  "Load specified MODULES."
  `(let ((modules (quote ,modules))
         category m)
     (while modules
       (setq m (pop modules))
       (cond
        ((keywordp m) (setq category m))
        ((not category) (error "No module category specified for %s" m))
        (t (load-module category m))))))


;; (macroexpand '(bootstrap :tools ivy))

;; (bootstrap :lang
;;            python
;;            :ui
;;            hl-todo
;;            treemacs
;;            :tools
;;            lookup)

(provide 'v-modules)
