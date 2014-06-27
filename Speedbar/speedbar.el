;; speedbar.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rameshk customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'speedbar)
(require 'sr-speedbar)

(defun speedbar-set-default-directory()
  "Interactive function to set current cursor point directory as default directory and refresh speedbar."
  (interactive)
  (speedbar-message (concat "Setting " (speedbar-line-directory) " as default directory."))
  (cd-absolute (speedbar-line-directory))
  (speedbar-refresh))

(defun windows-system-p()
  (if (member system-type '(windows-nt ms-dos))
	  t))

(defun speedbar-set-buffer-directory()
  "Interactive function to set current buffer directory as default in speedbar."
  (interactive)
  (cond
   ((unless (sr-speedbar-exist-p) (speedbar-message "speedbar doesn't exist")))
   ((if (not buffer-file-name)
	(speedbar-message "No file found")
      (progn
	(let ((current-buffer-path (file-name-directory buffer-file-name)))
	  (if current-buffer-path
	      (progn
		(speedbar-message (concat "Setting " current-buffer-path " as default directory"))
		(cd-absolute current-buffer-path)
		(speedbar-refresh))
	    (speedbar-message "No such directory"))))))))
;; Setting global set key as this function needs to be called from any buffer
(global-set-key "\C-cd" 'speedbar-set-buffer-directory)

(defun speedbar-goto-parent-directory()
  "Interactive function to go back from the current directory in speedbar and refresh speedbar."
  (interactive)
  (let* ((dir-separator-regex (if (windows-system-p)  ;; Setting directory separator based on operating system
				  "\\\\$" "\\\/$"))
	 (parent-directory (file-name-directory (replace-regexp-in-string dir-separator-regex "" default-directory))))  ;; getting parent directory
    (if parent-directory
	(progn
	  (cd-absolute parent-directory)
	  (speedbar-message (concat "Back to " parent-directory))
	  (speedbar-refresh))
      (speedbar-message "At root: can't go back"))))

;; Adding key binds to speedbar mode map
(define-key speedbar-mode-map "s" 'speedbar-set-default-directory)
(define-key speedbar-mode-map "P" 'speedbar-goto-parent-directory)
