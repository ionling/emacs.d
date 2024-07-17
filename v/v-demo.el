;;; v-demo.el --- Vision demo stuff -*- lexical-binding: t; -*-
;; Version: 20240716
;; Package-Requires: (notifications)
;;; Commentary:
;;; Code:
(require 'notifications)

;;;###autoload
(defun v-demo-an-error ()
  "Help function for simulating an error."
  (interactive)
  (error "An error"))


;;;###autoload
(defun v-demo-notify ()
  "Demo of `notifications-notify'."
  (interactive)
  (notifications-notify
   :title "Hello"
   :body "This is <b>important</b> <u>notification</u>."
   :actions '("Confirm" "I agree" "Refuse" "I disagree")
   :on-action 'v-demo-notify-ok))

(defun v-demo-notify-ok (id key)
  "Action of `v-demo-notify'.
The notification ID and the KEY of the action
are passed as arguments to the function."
  (message "Notify with id: %s and key: %s" id key))


(provide 'v-demo)
;;; v-demo.el ends here
