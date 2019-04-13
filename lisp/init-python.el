;;; Commentary:
;;; Code:
(use-package anaconda-mode
  :after python
  :diminish "Ay"
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :after anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda)
  )

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  )

(use-package yapfify
  :hook (python-mode . yapf-mode)
  )

(provide 'init-python)
;;; init-python.el ends here
