;;; init-modules.el --- Defines and manages enabled modules
;;; Commentary:
;;; Code:

(defvar v-mod-spell-checking-enabled nil)

;; TODO
(defcustom v-mod-lsp-enabled nil
  "Whether LSP is enabled or not."
  :group 'v-mod
  :type 'boolean)

(provide 'init-modules)
;;; init-modules.el ends here
