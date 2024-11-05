;;; v-unknown.el --- Some unknown functions or commands

;; Version: 20241108
;; Package-Requires: (expand-region go-playground request s smartparens)

;;; Commentary:

;; It's hard to put some functions or commands into a individual pacakage, so we have
;; this package.

;;; Code:
(require 'er-basic-expansions)
(require 'go-playground)
(require 'request)
(require 's)
(require 'smartparens)

(require 'v-org)
(require 'v-pkg)

(v-require projectile)

;;;; Douban
(defun v-get-url (url coding)
  "Parse the content from the given URL using CODING."
  (with-current-buffer (url-retrieve-synchronously url)
    (prog1
        (decode-coding-string (buffer-string) coding)
      (kill-buffer))))

(defvar v-parse-douban-short-comment-regex
  (rx
   "TalionData.commentList"
   (1+ anychar)
   "name: \""
   (group (1+ nonl))
   "\""
   (1+ anychar)
   "comment: '"
   (group (1+ nonl))
   "',")
  "Regex used by `v-douban-short-comment-to-org'.")

;;;###autoload
(defun v-douban-short-comment-to-org (url)
  "Parse short comments from the URL into org mode quote."
  (interactive "sURL: ")
  (url-retrieve
   url
   (lambda (status url buf)
     (let ((content (v-get-url url 'utf-8))
           redirect-url name comment matches)
       (setq redirect-url (plist-get status :redirect))
       (setq matches (s-match v-parse-douban-short-comment-regex content))
       (pop matches)
       (setq name (pop matches))
       (setq comment (pop matches))
       (with-current-buffer buf
         (insert (format "[[%s][%s]]:\n#+begin_quote\n%s\n#+end_quote"
                         redirect-url name comment)))))
   `(,url ,(current-buffer))))


;;;; sexp

;;;###autoload
(defun v-sexp-disable ()
  "Disable current sexp."
  (interactive)
  (sp-wrap-round)
  (insert "when nil\n"))

;;;###autoload
(defalias 'v-sexp-enable #'sp-raise-sexp
  "Enable current sexp.")


;;;; Visual Studio Code

;;;###autoload
(defun v-code-open-cur-file ()
  "Open current file in vscode."
  (interactive)
  (shell-command (concat "code " (buffer-file-name))))

;;;###autoload
(defun v-code-open-cur-project ()
  "Open current project in vscode."
  (interactive)
  (->> (projectile-project-root)
       (concat "code ")
       shell-command))

;;;; Misc

;;;###autoload
(defun v-edit-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;###autoload
(defun v-insert-current-date ()
  "Insert today's date.
Refer https://www.emacswiki.org/emacs/InsertingTodaysDate."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun v-link-markup (link format)
  "Call markup-link API use LINK and FORMAT params."
  (request "http://127.0.0.1:6666/api/markup-link"
    :type "POST"
    :data (json-encode `(("format" . ,format) ("link". ,link)))
    :parser 'json-read
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Got error: %S" error-thrown)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (insert (alist-get 'markup data))))))

;;;###autoload
(defun v-link-md-clipboard ()
  "Insert markdown link."
  (interactive)
  (v-link-markup (current-kill 0) 'md))

;;;###autoload
(defun v-link-org-clipboard ()
  "Insert `org-mode' link."
  (interactive)
  (v-link-markup (current-kill 0) 'org))

;;;###autoload
(defun v-org-babel-go-playground ()
  "Run current babel code in go playground."
  (interactive)
  (v-org-babel-copy-code)
  (go-playground)
  (search-backward-regexp "package main")
  (delete-region (point) (point-max))
  (yank))

;;;###autoload
(defun v-prog-func-count-lines ()
  "Count current function lines."
  (interactive)
  (save-mark-and-excursion
    (er/mark-defun)
    (message "Current function total %d lines"
             (count-lines (region-beginning) (region-end)))))

;;;###autoload
(defun v-wechat-clean-url (url)
  "Clean unnecessary query parameters in WeChat URL.
e.g. https://mp.weixin.qq.com/s?__biz=mzu5mzyznzmzma==&mid=2247483692&idx=1&sn=e0e25ba20f087d2dd8480c95f3ff2536&scene=21#wechat_redirect"
  (interactive "sURL: ")
  (let* ((splits (s-split-up-to "?" url 1))
         (base-url (-first-item splits))
         (query-str (-last-item splits)))
    (->> query-str
         url-parse-query-string
         ;; Keep params' order, as `url-parse-query-string' returns a reverse list
         reverse
         (-filter (lambda (kv) (-contains-p '("__biz" "mid" "idx" "sn") (car kv))))
         url-build-query-string
         (concat base-url "?")
         (kill-new)
         (message "Copied cleaned URL: %s"))))

;;;###autoload
(defun v-window-prev ()
  "Go to previous window."
  ;; REF https://emacsredux.com/blog/2013/03/30/go-back-to-previous-window/
  (interactive)
  (other-window -1))

(provide 'v-unknown)
;;; v-unknown.el ends here
