;;; init-coding.el --- Coding stuff
;;; Commentary:
;;; Code:
(require 'init-core)


(v-defun lang-find-definition
         "Find definitions of the symbol under point.")
(v-defun lang-find-references
         "Find references of the symbol under point.")
(v-defun lang-find-apropos
         "Find all meaningful symbols that match PATTERN")
(v-defun lang-sort-imports
         "Sort the region imports, or if none is select, the buffer.")
(v-defun lang-format
         "Format the region, or if none is select, the buffer.")

(setq v-lang-find-references-func #'xref-find-references)
(setq v-lang-find-definition-func #'xref-find-definitions)
(setq v-lang-find-apropos-func #'xref-find-apropos)
