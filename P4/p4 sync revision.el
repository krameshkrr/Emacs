;; P4 sync revision:
;; 
;; Emacs p4 plugin doesn't have provision to sync a file for given revision. p4-sync-revision enables to sync given revision of the current buffer file if its not opened for pending changelist.
;; 
;; p4-fstat-output
;; - Function to run fstat command and parse the output and returns matched value for given regex.
;; - Ex: 
;; 	- To get depot file : (p4-fstat-output "-T depotFile" "depotFile \\(.+\\)$")
;; 	- To get headRev: (p4-fstat-output "-T headRev" "headRev \\([0-9]+\\)$")
;; 
(defun p4-fstat-output (cmdarg regex &optional file-name match-occurrence)
  "Run fstat command and returns corresponding value which matches with given regex and its occurrence.
CMDARG : arguments to command fstat (ex: -T headRev).
REGEX : Regex to get required value from fstat output.
FILE-NAME: (Optional) if not given, current buffer file will get processed.
MATCH-OCCURRENCE: Matching occurrence of given regex."
  (let ((output-buffer (p4-depot-output "fstat" (list cmdarg (if file-name file-name (buffer-file-name)))))
        (return-val nil))
    (with-current-buffer output-buffer
      (if (not (string= "" regex))
          (progn
            (goto-char (point-min))
            (when (re-search-forward regex nil t match-occurrence)
              (setq return-val (match-string 1))))
        (setq return-val (buffer-string))))
    (kill-buffer output-buffer)
    return-val))

;; p4-get-head-revision
;;	- Runs fstat -T headRev and process output to get the head rev.

(defun p4-get-head-revision (&optional file-name)
  "Returns head revision of given or current buffer file."
  (p4-fstat-output "-T headRev" "headRev \\([0-9]+\\)"))


;; p4-sync-revision:
;; - It is a p4cmd function, which gets the file revision less than the head revision and sync the same.
;; - Test cases: 
;; 	- File should be controlled by perforce.
;; 	- file revision 0 is not allowed, since 0 is used to delete the file. Use p4-delete to delete the file.
;; 	- file revision should be less then or equal to head revision(fstat -T headRev).

(defp4cmd p4-sync-revision ()
  "sync" "To sync particular revision of the current file with depot, type \\[p4-sync-revision] 
  and enter file revision less than head revision shown.

File revision should be a valid number and less than head version of the file and
file shouldn't have opened for pending changelist by user.\n"
  (interactive)
  (let ((head-revision (p4-get-head-revision (p4-buffer-file-name))))
    (let ((revision (p4-read-arg-string (concat "p4 sync revision (<= " head-revision "): ")))
      (args (p4-buffer-file-name)))
    (cond
     ((not head-revision) (message "File is not controlled by perforce."))
     ((and revision
           (and (string-match "^[1-9][0-9]*$" revision) (<= (string-to-number revision) (string-to-number head-revision))))
      (progn
        (setq args (p4-make-list-from-string (concat args "#" revision)))
        (p4-noinput-buffer-action "sync" nil t args)
        (p4-refresh-files-in-buffers)
        (p4-make-depot-list-buffer
         (concat "*P4 Sync Revision: (" (p4-current-client) ") " (car args) "*"))))
     (t (message "Invalid file revision."))))))

