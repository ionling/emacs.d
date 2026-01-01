;;; vo-org.el --- vision org mode config           -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package v-org :v-ensure)

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

(use-package org-contrib)

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

(use-package ox-html :ensure nil
  :config
  ;; REF https://coldnew.github.io/a1ed40e3/
  (defadvice org-html-paragraph (before org-html-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line.
Without unwanted space when exporting `org-mode' to html."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

      (ad-set-arg 1 fixed-contents))))

(use-package ox-gfm)

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

(use-package org-visual-outline :ensure nil
  :disabled
  :quelpa (org-visual-outline :fetcher github :repo "legalnonsense/org-visual-outline"))

(use-package org-super-links :ensure nil
  :disabled
  :quelpa (org-super-links :fetcher github :repo "toshism/org-super-links"))

(use-package org-clock-reminder
  :defer 1
  :config (org-clock-reminder-mode))

(use-package org-super-agenda
  :hook (org-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '((:tag "weekly")
     (:tag "long")
     (:tag "project")
     (:tag "inbox")
     (:auto-category t))))


(provide 'vo-org)
;;; vo-org.el ends here
