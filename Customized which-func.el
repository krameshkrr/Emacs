;; Customized which-func mode
(require 'which-func)
(which-function-mode t)

;; Shorten the fully qualified name of the function
(defun shorten-function-name (name &optional max-length)
  "Shorten the function name to display in mode line when it exceeds the window width."
  (unless max-length
    (setq max-length 25))
  (let ((split-name (reverse (split-string name "::")))
        (short-name ""))
    (when (and split-name (equal "" (car split-name)))
      (setq split-name (cdr split-name)))
    (while (and split-name (< (length short-name) (- max-length 3)))
      (unless (string= short-name "")
          (setq short-name (concat ":" short-name)))
      (setq short-name (concat (car split-name) short-name))
      (setq split-name (cdr split-name)))
    (when short-name
      (setq short-name (concat "..." short-name)))
    short-name))

(defconst my-which-func-current
  `(:eval (shorten-function-name (replace-regexp-in-string
           "%" "%%"
           (gethash (selected-window) which-func-table which-func-unknown)))))

;; Replacing default which-func-current in which-func.el
(defun set-which-func-current ()
  (setq which-func-current 'my-which-func-current))
