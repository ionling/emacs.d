;;; Commentary:
;;; Code:
(use-package org
  :init (require 'org-man)
  :config
  (setq org-plantuml-jar-path
	(expand-file-name "plantuml.1.2018.13.jar" org-directory))
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  (setq org-todo-keyword-faces '(("DOING" . "purple")))
  (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (plantuml . t))))

(use-package org-brain
  :config
  (setq org-id-locations-file
	(expand-file-name ".org-id-locations" org-brain-path)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-cliplink)

(use-package org-mind-map
  :init (require 'ox-org))

(provide 'init-org)
;;; init-org.el ends here
