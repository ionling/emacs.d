;;; init-modules.el --- Defines and manages enabled modules
;;; Commentary:
;;; Code:

(defvar v-mod-spell-checking-enabled nil)

;; TODO
(defcustom v-mod-ivy-enabled t
  "Whether ivy is enabled or not."
  :group 'v-mod
  :type 'boolean)

;; TODO
(defcustom v-mod-lsp-enabled t
  "Whether LSP is enabled or not."
  :group 'v-mod
  :type 'boolean)

(provide 'modules)
;;; modules.el ends here
