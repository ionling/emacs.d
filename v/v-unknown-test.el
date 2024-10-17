;;; v-unknown-test.el --- Test for `v-unknown' package -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 'v-unknown)

(ert-deftest v-douban-short-comment-to-org ()
  (let ((url "https://www.douban.com/doubanapp/dispatch?uri=/movie/1418834/interest/1270216583")
        (expect (concat "[[https://m.douban.com/movie/comment/1270216583][宇宙人小早👽]]:\n"
                        "#+begin_quote\n"
                        "这么多年过去了，印象最深的并不是电影本身，是豆瓣评论区的第一个问题："
                        "”为什么恩尼斯看到杰克开车远离后，会在路边胸闷不止？"
                        "“答：“你爱一次就知道了。” 一针见血╮(╯▽╰)╭"
                        "\n#+end_quote")))
    (should (equal expect
                   (with-temp-buffer
                     ;; FIXME We should use `url-retrieve-synchronously', as:
                     ;;   error in process filter: save-current-buffer: Selecting deleted buffer
                     (v-douban-short-comment-to-org url)
                     (buffer-string))))))


(provide 'v-unknown-test)
;;; v-unknown-test.el ends here
