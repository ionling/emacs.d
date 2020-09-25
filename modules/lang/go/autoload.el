;; https://stackoverflow.com/a/48409550

;;;###autoload
(defun v-go-swag-init ()
  "Call `swag init` in project root."
  (interactive)
  ;; (shell-command) does use the the buffer's default-directory
  (let ((default-directory (projectile-project-root)))
    (shell-command "swag init" "*swag init*")))


