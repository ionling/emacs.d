;;; v-graphic.el --- Graphic display stuff -*- lexical-binding: t -*-

;; Version: 20211109

;;; Commentary:
;;; Code:
(require 'dash)
(require 'f)


(defvar v-frame-geometry-file
  (f-join user-emacs-directory "frame-geometry")
  "File to store frame geometry.")


;;;###autoload
(defun v-frame-geometry-load ()
  "Set selected frame geometry by data in `v-frame-geometry-file'."
  (interactive)
  (let ((geometry (car (read-from-string (f-read v-frame-geometry-file)))))
    (set-frame-position (selected-frame)
                        (alist-get 'left geometry)
                        (alist-get 'top geometry))
    (set-frame-size (selected-frame)
                    (alist-get 'width geometry)
                    (alist-get 'height geometry))))


;;;###autoload
(defun v-frame-geometry-save ()
  "Save selected frame geometry to `v-frame-geometry-file'."
  (interactive)
  (f-write
   (pp (-map
        (lambda (name)
          `(,name . ,(frame-parameter (selected-frame) name)))
        '(left top width height)))
   'utf-8 v-frame-geometry-file))


(provide 'v-graphic)
;;; v-graphic.el ends here
