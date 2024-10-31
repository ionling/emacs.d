;;; v-graphic.el --- Graphic display stuff -*- lexical-binding: t -*-

;; Version: 20241031

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

;; REF https://www.reddit.com/r/emacs/comments/9c0a4d/tip_setting_initial_frame_size_and_position/

;;;###autoload
(defun v-frame-init ()
  "Set initial frame size and position."
  (interactive)
  (let* ((base-factor 0.8)
         (width (* (display-pixel-width) base-factor))
         (height (* (display-pixel-height) base-factor))
         (left (-> (display-pixel-width) (- width) (/ 2) truncate))
         (top (-> (display-pixel-height) (- height) (/ 2) truncate)))
    (set-frame-position (selected-frame) left top)
    (set-frame-size (selected-frame) (truncate width)  (truncate height) t)))

(provide 'v-graphic)
;;; v-graphic.el ends here
