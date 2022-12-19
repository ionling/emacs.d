;;; v-unknown.el --- Some unknown functions or commands

;; Version: 20221220

;;; Commentary:

;; It's hard to put some functions or commands into a individual pacakage, so we have
;; this package.

;;; Code:
(require 'v-pkg)
(v-require projectile)


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


(provide 'v-unknown)
;;; v-unknown.el ends here
