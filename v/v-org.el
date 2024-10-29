;;; v-org.el --- vision org mode config           -*- lexical-binding: t -*-

;; Author: Vision Ling
;; Homepage: https://github.com/ionling/emacs.d
;; Keywords: configuration org-mode
;; Version: 20241017
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
      (use-package valign
        :if (display-graphic-p))

      (use-package org
        :custom
        (org-archive-location "~/org/datetree.org::datetree/")
        (org-footnote-section nil "Convenient when moving a subtree")
        (org-image-actual-width nil)
        (org-log-done 'time)
        (org-modules '(ol-info org-id))
        (org-startup-indented t)
        (org-hide-block-startup nil)
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
        (setq org-plantuml-jar-path (expand-file-name "plantuml.1.2019.12.jar" org-directory)
              org-agenda-files `(,(expand-file-name "agenda" org-directory)
                                 ,(expand-file-name "todo.org" org-directory))
              org-clock-in-switch-to-state "DOING"
              org-clock-rounding-minutes 5
              org-clock-sound t
              org-log-into-drawer t
              org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE(d)" "ABORT"))
              org-todo-keyword-faces '(("DOING" . "purple") ("ABORT" . "sea green"))
              org-show-notification-timeout 8
              org-src-lang-modes (append org-src-lang-modes
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

      (use-package org-capture :ensure nil
        :custom
        (org-capture-templates
         '(("i" "Inbox" entry (file "~/org/in.org")
            "* %U %^{Heading}\n%?" :prepend t)
           ("n" "Notes" entry (file "~/org/x.org")
            "* %^{heading}%?\n" :prepend t))))

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

      (use-package ob-go
        :after org
        :init
        (add-to-list 'org-babel-load-languages '(go . t))
        (org-babel-do-load-languages
         'org-babel-load-languages org-babel-load-languages))

      (use-package ox-hugo
        :custom
        (org-hugo-auto-set-lastmod t))

      (use-package org-cliplink
        :general
        (v-org-map "l" #'org-cliplink))

      (use-package org-mind-map
        :init (require 'ox-org))

      (use-package org-tanglesync
        :disabled)

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


;;;; Babel

;;;###autoload
(defun v-org-babel-load-lang ()
  "Load lang of current source block."
  (interactive)
  (if-let ((info (org-babel-get-src-block-info))
           (lang (car info)))
      (require (intern (concat "ob-" lang)))
    (user-error "No src block here")))

;;;###autoload
(defun v-org-babel-tangle-tmp ()
  "Tangle the block at point to tmp file."
  (interactive)
  (org-babel-tangle 4))


;;;###autoload
(defun v-org-babel-tangle-tmp2 ()
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
  "Goto an org heading."
  (interactive)
  (ivy-read "Go " (cl-concatenate 'list (v-org-outline))
            :action #'v-org-goto-action))

;;;###autoload
(defun v-org-goto-indirect ()
  "Goto an org heading in an indirect buffer."
  (interactive)
  (ivy-read "Go " (cl-concatenate 'list (v-org-outline))
            :action #'v-org-goto-indirect-action))

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

(defun v-org-goto-indirect-action (x)
  "Goto Org header defined in X."
  (let ((info (cdr x)))
    (v-org-goto-action x)
    (message "v org goto %s" info)      ; Magic line
    (v-org-subtree-indirect-buffer)))

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
              (title (plist-get x 'title))
              (tag (plist-get x 'tag)))
          (when tag
            (setq title (concat title " " tag)))
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

(defvar v-org-outline-rg-regexp
  (rx
   (group (1+ nonl))                    ; filename
   ":"
   (group (1+ num))                     ; line number
   ":"
   (group (1+ "*"))                     ; level
   " "
   (group  (+? nonl))                   ; title
   (? (group ":" (1+ nonl) ":"))        ; tags
   eol)
  "The regexp for parsing the rigrep line.")

(defmacro pop-to-plist (list plist property &optional convert)
  "Pop element in LIST to PLIST with PROPERTY.
If CONVERT is not nil, use it to transform the popped value."
  `(let ((v (pop ,list)))
     (if ,convert
         (setq v (funcall ,convert v)))
     (setq ,plist (plist-put ,plist ,property v))))

;; OPTI Benchmark it, or speed it up by compile regexp
(defun v-org-outline-parse-rg (line)
  "Parse a ripgrep LINE.
e.g.: './a.org:202:*** video 4'"
  (let* ((matches (s-match v-org-outline-rg-regexp line))
         plist)
    (pop matches)
    (pop-to-plist matches plist 'file)
    (pop-to-plist matches plist 'line #'string-to-number)
    (pop-to-plist matches plist 'level #'length)
    (pop-to-plist matches plist 'title #'s-trim)
    (pop-to-plist matches plist 'tag)
    (pop-to-plist matches plist 'remain)
    plist))


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


(defvar v-org-clock-end-regex
  (rx "--"
      (group-n 1
        "["
        (repeat 4 digit) "-"            ; YYYY-
        (repeat 2 digit) "-"            ; MM-
        (repeat 2 digit) " "            ; DD
        (one-or-more (in "A-Za-z")) " " ; Day
        (repeat 2 digit) ":"            ; HH:
        (repeat 2 digit)                ; MM
        "]")))

(defun v-org-clock-get-last-end ()
  "Get the end time of the last clock entry of the current subtree.
By GPT40."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t))))
      (re-search-forward org-clock-string end t)
      (let ((clock-line (thing-at-point 'line t)))
        (if (string-match v-org-clock-end-regex clock-line)
            (match-string 1 clock-line))))))


(defun v-org-time-inactive (&optional time zone)
  "Generate an inactive timestamp according to TIME and ZONE."
  (format-time-string "[%Y-%m-%d %a %H:%M]" time zone))


;;;###autoload
(defun v-org-todo-set-done-time ()
  "Set the CLOSED timestamp of the current todo entry to DONE-TIME."
  (interactive)
  (if (org-get-todo-state)  ;; Ensure it's a todo entry
      (org-todo "DONE")
    (user-error "Not a todo entry"))

  (let ((done-time
         (or (v-org-clock-get-last-end)
             (with-temp-buffer (v-org-time-inactive)))))
    (save-excursion
      (let ((end (save-excursion (org-end-of-subtree t t))))
        (org-back-to-heading t)
        (cond
         ((re-search-forward org-closed-time-regexp end t)
          (replace-match (concat "CLOSED: " done-time) t t))
         ((re-search-forward org-scheduled-time-regexp end t)
          (replace-match (concat "CLOSED: " done-time " " (match-string 0)) t t))
         (t
          (end-of-line)
          (insert (concat "\nCLOSED: " done-time "\n"))))))))


(defvar v-org-diary-workflowy-line-regex
  (rx "- " (group (+ num)) " " (group (+ nonl)))
  "Regex used to match diary line in the WorkfFlowy.
Used in `v-org-diary-from-workflowy'.")

;;;###autoload
(defun v-org-diary-from-workflowy (diary &optional rev)
  "Import DIARY from the WorkfFlowy, and insert it in the org format.
If REV the insert order will be reversed."
  (interactive "*s\nP")
  (-as-> diary it
         (s-lines it)
         (-filter (-not #'string-blank-p) it)
         (if rev (reverse it) it)
         (-map (lambda (x) (cdr (s-match v-org-diary-workflowy-line-regex x))) it)
         (dolist (line it)
           (org-insert-heading)
           (insert (-first-item line))
           (insert "\n")
           (insert (-second-item line)))))

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
(defun v-org-property-set-created-at ()
  "Set CREATED_AT property of current entry."
  (interactive)
  (org-set-property
   "CREATED_AT"
   (with-temp-buffer
     (org-insert-time-stamp (current-time) t t))))

;;;###autoload
(defun v-org-property-set-douban-short-commented-at ()
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

;;;###autoload
(defun v-org-counsel-fzf ()
  "Open a file in `org-directory' using the `counsel-fzf'."
  (interactive)
  (counsel-fzf nil org-directory))

(provide 'v-org)
;;; v-org.el ends here
