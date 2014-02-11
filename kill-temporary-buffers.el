;; Killing temporary buffers
(defun kill-unnecessary-buffers ()
  "Killing unnecessary buffers such as *compilation*, *P4-output* etc.,"
  (interactive)
  (let* ((list-of-buffers (mapcar (function buffer-name) (buffer-list)))
         (list-of-valid-buffers '("*scratch*" "*Messages*"))
         (current-buffer-name nil))
    (while list-of-buffers
      (setq current-buffer-name (car list-of-buffers))
      (when (and (string-match "^\*.+\*$" current-buffer-name) (not (member current-buffer-name list-of-valid-buffers)))
        (message (concat "Killing buffer: " current-buffer-name))
        (kill-buffer current-buffer-name))
      (setq list-of-buffers (cdr list-of-buffers)))
    (message "Killed all temporary buffers.")))

(global-set-key "\M-K" 'kill-unnecessary-buffers)
