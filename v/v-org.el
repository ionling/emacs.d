;;; v-org.el --- vision org mode config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration org-mode
;; Version: 20230520
;; Package-Requires: (org org-cliplink dash f s counsel)

;;; Commentary:

;;; Code:
(require 'org)
(require 'org-element)

(require 'counsel)
(require 'dash)
(require 'f)
(require 'org-cliplink)
(require 's)


;;;###autoload
(defun v-org-config ()
  "Load org mode config."
  (interactive)
  (eval
   '(progn
      (use-package org
        :custom
        (org-archive-location "~/org/datetree.org::datetree/")
        (org-footnote-section nil "Convenient when moving a subtree")
        (org-image-actual-width nil)
        (org-log-done 'time)
        (org-modules '(ol-info org-id))
        (org-startup-indented t)
        (org-hide-block-startup t)
        :general
        (vision-map
         :prefix "o"
         :prefix-command 'v-org-map
         "a" #'org-agenda
         "i" #'org-clock-in
         "o" #'org-clock-out
         "g" #'v-org-goto
         "s" #'v-org-search
         "b" #'v-org-subtree-indirect-buffer)
        :config
        (setq org-plantuml-jar-path
              (expand-file-name "plantuml.1.2019.12.jar" org-directory)
              org-agenda-files `(,(expand-file-name "agenda" org-directory))
              org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE(d)" "ABORT"))
              org-todo-keyword-faces '(("DOING" . "purple") ("ABORT" . "sea green"))
              org-src-lang-modes
              (append org-src-lang-modes
                      '(("less" . less-css)
                        ("py" . python)
                        ("puml" . plantuml)
                        ("toml" . conf-toml)
                        ("zsh" . sh))))
        (defalias 'org-babel-execute:py 'org-babel-execute:python)
        (defalias 'org-babel-execute:puml 'org-babel-execute:plantuml)
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((emacs-lisp . t)
           (plantuml . t)
           (python . t)
           (shell . t))))

      (use-package org-id :ensure nil
        :custom
        (org-id-link-to-org-use-id t))

      ;; `org-indent-mode' is enabled by setting the `org-startup-indented' to `t'.
      (use-package org-indent :ensure nil :delight
        :custom
        (org-indent-indentation-per-level 1))

      (use-package org-src :ensure nil
        :custom
        (org-src-window-setup 'other-window))

      (use-package org-cliplink
        :general
        (v-org-map "l" #'org-cliplink))

      (use-package org-super-agenda
        :hook (org-mode . org-super-agenda-mode)
        :custom
        (org-super-agenda-groups
         '((:tag "weekly")
           (:auto-category t)))))))

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
(defun org-babel-tangle-tmp ()
  "Tangle the block at point to tmp file."
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
(defun v-org-babel-copy-code ()
  "Save current babel code to kill ring."
  (interactive)
  (if-let ((info (org-babel-get-src-block-info))
           (code (nth 1 info)))
      (kill-new code)
    (user-error "No src block here")))


;;;; Outline commands:

;;;###autoload
(defun v-org-goto ()
  "Goto org heading."
  (interactive)
  (ivy-read "Go " (cl-concatenate 'list (v-org-outline))
            :action #'v-org-goto-action))


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


(defun v-org-outline (&rest paths)
  "Get all headings of all org files in the PATHS.
The element of PATHS can be either a file or a directory.
If PATHS is empty, `org-directory' is used as the default path."
  (if (= (length paths) 0)
      (push org-directory paths))
  (let* ((cmd (format "rg -H -n -g '*.org' '^\\*+ .+' %s" (s-join " " paths)))
         (output (shell-command-to-string cmd))
         (last-level 0)
         ;; level-title: `'(1 title1 2 title2)`
         level-titles)
    (->>
     (split-string output "\n" t)
     (mapcar #'v-org-outline-parse-rg)
     (mapcar
      (lambda (x)
        (let ((file (plist-get x 'file))
              (line (plist-get x 'line))
              (level (plist-get x 'level))
              (title (plist-get x 'title)))
          ;; Delete title when level up
          (-map (lambda (x)
                  (setq level-titles (plist-put level-titles x "")))
                (number-sequence level last-level))
          (setq last-level level)
          (setq level-titles (plist-put level-titles level title))
          (let* ((file-heading (->> file
                                    (s-replace (f-expand org-directory) "")
                                    (f-no-ext)))
                 ;; Headline path
                 (path (->> (number-sequence 1 level)
                            (-map (lambda (y) (plist-get level-titles y)))
                            (s-join "/")
                            (concat file-heading "/"))))
            `(,path . (,file . ,line)))))))))

(defun v-org-outline-parse-rg (line)
  "Parse a ripgrep LINE.
e.g.: './a.org:202:*** video 4'"
  (->> line
       (s-match
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Rx-Constructs.html
        (rx
         (group (1+ nonl))
         ":"
         (group (1+ num))
         ":"
         (group (1+ "*"))
         " "
         (group (1+ nonl))))
       ((lambda (matches)
          `(file
            ,(-second-item matches)
            line
            ,(string-to-number (-third-item matches))
            level
            ,(length (-fourth-item matches))
            title
            ,(-fifth-item matches))))))


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


;;;; Misc

(defun v-org-cliplink-markdown-mode-link-tansformer (url title)
  "Generate a Markdown link based on the given URL and TITLE.
The TITLE will be transformed by certain rules,
like `org-cliplink-org-mode-link-transformer'."
  (if (not title)
      url
    (format "[%s](%s)"
            (org-cliplink-elide-string
             (org-cliplink-escape-html4
              (org-cliplink-title-for-url url title))
             org-cliplink-max-length)
            url)))

;;;###autoload
(defun v-org-cliplink-markdown ()
  "The Markdown version of `org-cliplink'."
  (interactive)
  (org-cliplink-insert-transformed-title
   (org-cliplink-clipboard-content)
   #'v-org-cliplink-markdown-mode-link-tansformer))

;;;###autoload
(defun v-org-link-show ()
  "Show the raw-link of the org link element at point."
  (interactive)
  (let* ((el (org-element-context))
         (type (org-element-type el)))
    (if (not (eq type 'link))
        (message "Not a link, but a %s" type)
      (message "Link: %s" (org-element-property :raw-link el)))))

;;;###autoload
(defun v-org-set-property-created-at ()
  "Set CREATED_AT property of current entry."
  (interactive)
  (org-set-property
   "CREATED_AT"
   (with-temp-buffer
     (org-insert-time-stamp (current-time) t t))))

;;;###autoload
(defun v-org-set-property-douban-short-commented-at ()
  "Set DOUBAN_SHORT_COMMENTED_AT property of current entry."
  (interactive)
  (org-set-property
   "DOUBAN_SHORT_COMMENTED_AT"
   (with-temp-buffer
     (org-insert-time-stamp (current-time) t t))))


;; e.g.:
;; 如何看待 2022 年春节期间三、四线城市新房成交下跌超九成？ - 王克丹的回答 - 知乎
;; https://www.zhihu.com/question/516693284/answer/2351591652
;;;###autoload
(defun v-org-yank-zhihu-sharing ()
  "Yank Zhihu sharing."
  (interactive)
  (let* ((str  (current-kill 0))
         (arr (s-lines str))
         (title (-first-item arr))
         (link (-last-item arr)))
    (-> (format "[[%s][%s]]" link title)
        (insert))))


(provide 'v-org)
;;; v-org.el ends here
