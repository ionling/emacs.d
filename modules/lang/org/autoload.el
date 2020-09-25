
;;;###autoload
(defun org-x-goto ()
  "Find X entry use `counsel-outline'."
  (interactive)
  (find-file "~/org/x.org")
  (counsel-outline))


;;;###autoload
(defun org-tree-indirect-buffer ()
  "Create indirect buffer and narrow it to current subtree without close last."
  (interactive)
  (let ((headline (org-get-heading t t t t)))
    (make-indirect-buffer (current-buffer) headline t)
    (switch-to-buffer headline)
    (org-narrow-to-subtree)))



;;;###autoload
(defun org-x-goto-indirect-buffer ()
  "Create indirect buffer for X entry."
  (interactive)
  (find-file "~/org/x.org")
  (let ((rand (number-to-string (random 999))))
    (make-indirect-buffer (current-buffer) rand t)
    (switch-to-buffer rand)
    (unwind-protect
        (progn (counsel-outline)
               (org-narrow-to-subtree)
               (let ((headline (org-get-heading t t t t)))
                 (rename-buffer headline)))
      (kill-buffer rand))))



;;;###autoload
(defun org-x-search ()
  (interactive)
  (find-file "~/org/x.org")
  (->> (kbd "C-s")
       (lookup-key (current-global-map))
       funcall))



;;;###autoload
(defun org-demote-top-headlines ()
  "Demote all top level headlines in current buffer."
  (interactive)
  (org-map-entries #'org-demote-subtree "LEVEL=1"))


;;;###autoload
(defun org-promote-2-level-headlines ()
  "Promote all headlines 2 levels"
  (interactive)
  (org-map-entries #'org-promote-subtree "LEVEL=2"))


;;;###autoload
(defun org-babel-tangle-tmp ()
  "Tangle the block at point to tmp file"
  (interactive)
  (org-babel-tangle 4))


;;;###autoload
(defun org-babel-tangle-tmp2 ()
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (lang (car info))
         (code (nth 1 info)))
    (message code)
    (-> (make-temp-file "" nil (concat "." lang) code)
        (find-file))))


;;;###autoload
(defun org-babel-copy-code ()
  (interactive)
  (if-let ((info (org-babel-get-src-block-info))
           (code (nth 1 info)))
      (kill-new code)
    (user-error "No src block here")))


(defun v-org-outline (file)
  (let* ((cmd (format "rg -n '^\\*+ ' %s" file))
         (output (shell-command-to-string cmd))
         (last-level 0)
         level-title)
    (->>
     (split-string output "\n" t)
     (mapcar
      (lambda (x)
        (let ((index-of-colon (s-index-of ":" x))
              (index-of-space (s-index-of " " x)))
          `(line ,(->> index-of-colon
                       (substring x 0)
                       (string-to-number))
                 level ,(length (substring x (+ index-of-colon 1) index-of-space))
                 title ,(substring x (+ index-of-space 1))))))
     (mapcar
      (lambda (x)
        (let ((level (plist-get x 'level))
              (title (plist-get x 'title))
              (line (plist-get x 'line)))
          ;; Delete title when level up
          (mapcar (lambda (x)
                    (setq level-title (plist-put level-title x "")))
                  (number-sequence level last-level))
          (setq last-level level)
          (setq level-title (plist-put level-title level title))
          (let* ((file-base (file-name-base file))
                 (path (->> (number-sequence 1 level)
                            (mapcar (lambda (y) (plist-get level-title y)))
                            (s-join "/")
                            (format "%s/%s" file-base))))
            `(,path . (,file . ,line)))))))))


(defun v-org-goto-action (x)
  "Goto Org header defined in X."
  (let* ((info (cdr x))
         (file (car info))
         (line (cdr info)))
    (find-file file)
    (goto-line line)))


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
