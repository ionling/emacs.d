;;; v-demo.el --- Vision demo stuff -*- lexical-binding: t; -*-
;; Version: 20220115
;;; Commentary:
;;; Code:

;;;###autoload
(defun v-demo-an-error ()
  "Help function for simulating an error."
  (interactive)
  (error "An error"))

(provide 'v-demo)
;;; v-demo.el ends here
