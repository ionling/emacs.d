;;; v-org-test.el --- Test for `v-org' package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 'v-org)

(ert-deftest v-org-outline-parse-rg ()
  (let ((res (v-org-outline-parse-rg "./a.org:202:*** video 4")))
    (should (equal (plist-get res 'file) "./a.org"))
    (should (equal (plist-get res 'line) 202))
    (should (equal (plist-get res 'level) 3))
    (should (equal (plist-get res 'title) "video 4"))))

(provide 'v-org-test)

;;; v-org-test.el ends here
