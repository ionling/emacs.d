;;; v-org.el --- vision org mode config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration org-mode
;; Package-Requires: (org)

;;; Commentary:

;;; Code:

(require 'counsel)
(require 'dash)
(require 'org)
(require 's)



;;;; Outline commands:

;;;###autoload
(defun v-org-goto ()
  "Goto org heading."
  (interactive)
  (let* ((files (directory-files "~/org" t "\\.org"))
         (cands (->>
                 files
                 (-map 'v-org-outline)
                 (-flatten-n 1))))
    (ivy-read "Go " cands
              :action #'v-org-goto-action)))


;;;###autoload
(defun v-org-goto-other-window ()
  "Goto org heading in another window."
  (interactive)
  (other-window 1)
  (v-org-goto))


;;;###autoload
(defun v-org-search ()
  "Search `org-directory'."
  (interactive)
  (counsel-rg "" org-directory))


(defun v-org-goto-action (x)
  "Goto org header defined in X."
  (let* ((info (cdr x))
         (file (car info))
         (line (cdr info)))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))


(defun v-org-outline (file)
  "Parse org FILE use rg."
  (let* ((cmd (format "rg -n '^\\*+ ' %s" file))
         (output (shell-command-to-string cmd))
         (last-level 0)
         ;; level-title: `'(1 title1 2 title2)`
         level-titles)
    (->>
     ;; A line is like this: `10690:**** 成都`
     (split-string output "\n" t)
     (mapcar
      (lambda (x)
        (let ((index-of-colon (s-index-of ":" x))
              (index-of-space (s-index-of " " x)))
          `(line
            ,(->> (substring x 0 index-of-colon)
                  (string-to-number))
            level
            ,(->> index-of-space
                  (substring x (+ index-of-colon 1))
                  (length))
            title
            ,(substring x (+ index-of-space 1))))))
     (mapcar
      (lambda (x)
        (let ((level (plist-get x 'level))
              (title (plist-get x 'title))
              (line (plist-get x 'line)))
          ;; Delete title when level up
          (-map (lambda (x)
                  (setq level-titles (plist-put level-titles x "")))
                (number-sequence level last-level))
          (setq last-level level)
          (setq level-titles (plist-put level-titles level title))
          (let* ((file-base (file-name-base file))
                 ;; Headline path
                 (path (->> (number-sequence 1 level)
                            (-map (lambda (y) (plist-get level-titles y)))
                            (s-join "/")
                            (format "%s/%s" file-base))))
            `(,path . (,file . ,line)))))))))



;;;; Subtree commands:

;;;###autoload
(defun v-org-subtree-demote-level1 ()
  "Demote all top level headlines in current buffer."
  (interactive)
  (org-map-entries #'org-demote-subtree "LEVEL=1"))


;;;###autoload
(defun v-org-subtree-promote-level2 ()
  "Promote all 2 level headlines in current buffer."
  (interactive)
  (let ((headline-points))
    (org-map-entries
     (lambda ()
       (push (point) headline-points))
     "LEVEL=2")
    (dolist (i headline-points)
      (goto-char i)
      (org-promote-subtree))))


;;;###autoload
(defun v-org-subtree-indirect-buffer ()
  "Create indirect buffer and narrow it to current subtree without close last."
  (interactive)
  (let ((headline (org-get-heading t t t t)))
    (make-indirect-buffer (current-buffer) headline t)
    (switch-to-buffer headline)
    (org-narrow-to-subtree)))


(provide 'v-org)
;;; v-org.el ends here
