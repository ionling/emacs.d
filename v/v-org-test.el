;;; v-org-test.el --- Test for `v-org' package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 's)
(require 'v-org)

(ert-deftest v-org-outline-rg-regexp ()
  (let* ((input "org/ledger.org:1:** Overview")
         (matches (s-match v-org-outline-rg-regexp input)))
    (should (equal (pop matches) input))
    (should (equal (pop matches) "org/ledger.org"))
    (should (equal (pop matches) "1"))
    (should (equal (pop matches) "**"))
    (should (equal (pop matches) "Overview")))

  (let* ((input "fsfs.org:657:** DONE Can not withdraw         :google:bug:")
         (matches (s-match v-org-outline-rg-regexp input)))
    (should (equal (pop matches) input))
    (should (equal (pop matches) "fsfs.org"))
    (should (equal (pop matches) "657"))
    (should (equal (pop matches) "**"))
    (should (equal (pop matches) "DONE Can not withdraw         "))
    (should (equal (pop matches) ":google:bug:"))))

(ert-deftest v-org-outline-parse-rg ()
  (let ((res (v-org-outline-parse-rg "./a.org:202:*** video 4")))
    (should (equal (plist-get res 'file) "./a.org"))
    (should (equal (plist-get res 'line) 202))
    (should (equal (plist-get res 'level) 3))
    (should (equal (plist-get res 'title) "video 4"))))

(when nil
  (setq l '(1 2 "***" 4))
  (setq p nil)
  (pop-to-plist l p 'file)
  (pop-to-plist l p 'name)
  (pop-to-plist l p 'level #'length)
  (if #'length
      (message "it")))

(provide 'v-org-test)

;;; v-org-test.el ends here
