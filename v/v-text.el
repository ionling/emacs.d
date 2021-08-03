;;; v-text.el --- Text manipulation -*- lexical-binding: t -*-

;; Version: 20210803
;; Package-Requires: (evil)

;;; Commentary:

;;; Code:
(require 'evil-states)


;;;###autoload
(defun v-text-punctuation-zh2en ()
  "Convert Chinese punctuations in region to English."
  (interactive)
  (let* ((range (evil-visual-range))
         (beg (car range))
         (end (cadr range)))
    (dolist (pair '(("，" . ", ")
                    ("。" . ". ")
                    ("：" . ": ")
                    ("、" . ", ")
                    ("（" . " (")
                    ("）" . ") ")))
      (let ((last-point beg)
            (old (car pair))
            (new (cdr pair)))
        (goto-char last-point)
        (while (< last-point end)
          (setq last-point (search-forward old end t))
          (if (eq last-point nil)
              (setq last-point (1+ end))
            (replace-match new)
            (setq end (+ end
                         (- (length new) (length old))))))))))


(provide 'v-text)
;;; v-text.el ends here
