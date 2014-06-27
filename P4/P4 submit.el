;; Instead of checking all the files opened in perforce, modified this function to check for given file if we pass the file name to this function.
(defun p4-empty-diff-p (&optional file-name)
  "Return t if there exists a file opened for edit or given file-name with an empty diff"
  (let ((buffer (get-buffer-create "p4-edp-buf"))
<div class=modified>	(regex-to-match-file "^\\(.*\\)#[0-9]* - edit.*")</div>
    opened empty-diff)
    (p4-exec-p4 buffer (list "opened") t)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (setq regex-to-match-file nil)
      (if file-name
          (setq regex-to-match-file (concat "\\(.*?" file-name "\\)#[0-9]* - edit.*")))

      (while (re-search-forward regex-to-match-file nil t)
    (setq opened (cons (match-string 1) opened))))
    (if (and file-name (not opened))
        (progn
        (setq empty-diff t)
        (goto-char (point-max)))
    (if opened
    (progn
      (p4-exec-p4 buffer (list "diff") t)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "====\n")
        (goto-char (point-min))
        (while (re-search-forward "^==== \\([^#\n]+\\)#.*\n====" nil t)
          (if (member (match-string 1) opened)
          (progn
            (setq empty-diff t)
            (goto-char (point-max)))))))))
    (kill-buffer buffer)
    empty-diff))
</codeblock>

;; function to delete non matching files when submiting a file
(defun p4-submit-set-current-file (file-name)
  "Deleting the unnecessary files from Files part in submit form"
  ;; Matching with depot version of files since submit form Files start with //depot path
  (let ((file-to-be-matched (p4-fstat-output "-T depotFile" "depotFile \\(.+\\)$" file-name)))
    (save-excursion ;; So point isn't moved after this function
      (goto-char (point-min))  ;;  Start at the beginning of the buffer
      (re-search-forward "Files:\n") ;; Go to the File part of the submit form
      (delete-non-matching-lines file-to-be-matched))))  ;; deleting non matching lines

(defun p4-async-process-command (p4-this-command &optional
                         p4-regexp
                         p4-this-buffer
                         p4-out-command
                         p4-in-args
                         p4-out-args)
  "Internal function to call an asynchronous process with a local buffer,
instead of calling an external client editor to run within emacs.

Arguments:
P4-THIS-COMMAND is the command that called this internal function.

P4-REGEXP is the optional regular expression to search for to set the cursor
on.

P4-THIS-BUFFER is the optional buffer to create. (Default is *P4 <command>*).

P4-OUT-COMMAND is the optional command that will be used as the command to
be called when `p4-async-call-process' is called.

P4-IN-ARGS is the optional argument passed that will be used as the list of
arguments to the P4-THIS-COMMAND.

P4-OUT-ARGS is the optional argument passed that will be used as the list of
arguments to P4-OUT-COMMAND."
  (let ((dir default-directory))
    (if p4-this-buffer
    (set-buffer (get-buffer-create p4-this-buffer))
      (set-buffer (get-buffer-create (concat "*P4 " p4-this-command "*"))))
    (setq p4-current-command p4-this-command)
    (cd dir))
  (if (zerop (apply 'call-process-region (point-min) (point-max)
            (p4-check-p4-executable) t t nil
            "-d" default-directory
            p4-current-command "-o"
            p4-in-args))
      (progn
    (goto-char (point-min))
    (insert (concat "# Created using " (p4-emacs-version) ".\n"
            "# Type C-c C-c to submit changes and exit buffer.\n"
            "# Type C-x k to kill current changes.\n"
            "#\n"))
    ;; Removing unnecessary files that were opened to submit
    (if (and (string= p4-out-command "submit") p4-out-args)
        (progn
        (p4-submit-set-current-file p4-out-args)
        (setq p4-out-args nil))) 
    (if p4-regexp (re-search-forward p4-regexp))
    (indented-text-mode)
    (setq p4-async-minor-mode t)
    (setq fill-column 79)
    (p4-push-window-config)
    (switch-to-buffer-other-window (current-buffer))
    (if p4-out-command
        (setq p4-current-command p4-out-command))
    (setq p4-current-args p4-out-args)
    (setq buffer-offer-save t)
    (define-key p4-async-minor-map "\C-c\C-c" 'p4-async-call-process)
    (run-hooks 'p4-async-command-hook)
    (set-buffer-modified-p nil)
    (message "C-c C-c to finish editing and exit buffer."))
    (error "%s %s -o failed to complete successfully."
       (p4-check-p4-executable) p4-current-command)))

;; The p4 submit command
(defp4cmd p4-submit (&optional arg)
  "submit" "To submit a pending change to the depot, type \\[p4-submit].\n"
  (interactive "P")
  (let (args
    (submit-buf-name "*P4 Submit*")
    (change-list (if (integerp arg) arg))
    (file-version (p4-is-vc nil (p4-buffer-file-name)))
    (p4-depot-file-name (p4-fstat-output "-T depotFile" "depotFile \\(.+\\)$"))
    (file-name (p4-buffer-file-name))
    (has-empty-diff nil))
    (if (buffer-live-p (get-buffer submit-buf-name))
    (switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if change-list
      (setq args (list "-c" (int-to-string change-list)))
    (if current-prefix-arg
        (setq args (p4-make-list-from-string
            (p4-read-arg-string "p4 submit: " nil)))))
      (setq args (p4-filter-out (lambda (x) (string= x "-c")) args))

      (p4-save-opened-files)
	  
      ;; Test cases:
        ;; 1. File is not in perforce => all opened files will be showed in submit form
        ;; 2. File is in perforce but not opened for edit or some other operation => It will prompt for empty diff
        ;; 3. File is opened for edit and it has no diff => It will prompt for empty diff
        ;; 4. File is opened for edit and it has diff =>  Allows you to submit this current file only.

      ;; check for empty diff if its opened for edit
      (if file-version
          (if (not (string= file-version "Add"))
          (progn
            (setq has-empty-diff
                  (and p4-check-empty-diffs
                       (p4-empty-diff-p
                        (replace-regexp-in-string p4-default-depot-completion-prefix "" p4-depot-file-name))))
            ;; If there is no diff, then we intentionally try to submit the file other than the current file.
            (if has-empty-diff
                (setq file-name nil))))
        (setq file-name nil))

      (if (or (not has-empty-diff)
              (progn
                (ding t)
                (yes-or-no-p
                 "File with empty diff opened for edit. Submit anyway? ")))
          (p4-async-process-command "change" "Description:\n\t"
                                    submit-buf-name "submit" args file-name)))))
