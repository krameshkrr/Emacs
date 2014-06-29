;;; kb-mode.el --- Summary

;;; Author: Ramesh Kandasamy <krameshkrr@gmail.com>
;;; Created: Jun 2014
;;; Version: 1.0
;;; Keywords: convenience 

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program ; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; Commentary:
; kb-mode (kill-buffer minor mode)
; Minor mode for managing temporary buffers which enables to kill temporary buffers easily by providing user-friendly functions to select buffers to be killed more importanytly timer object can be set to kill the temporary buffers automatically in given interval.

; Install
; Copy kb-mode.el to your load-path and add below snippet to your emacs init file.

;     (require 'kb-mode)
;     (kb-minor-mode 1)
; Customizations
; The below variables can be customized for your emacs session.

; Variables
; kb-default-temp-buffer-regex "\\*.+\\*"
;   "String: Default regex to match temp buffers."

; kb-kill-indirect-buffers nil
;   "Boolean: Flag to indicate whether or not to kill indirect buffers."

; kb-kill-if-modified nil
;   "Boolean: Flag to indicate whether or not to kill the buffer if it has modifed content."

; kb-prompt-for-running-process nil
;   "Boolean: Flag to indicate whether or not to prompt user when killing a buffer in which a process run."

; kb-kill-anyway nil 
;   "Boolean: Flag to indicate whether or not kill the buffers regardless of its state such as modified, indirect buffer etc."

; kb-clean-periodically t
;   "Boolean: Flag to indicate whether or not to run temp buffer cleaning periodically."

; kb-clean-interval 1000
;   "Number: Time interval to run temp buffer cleaning."
; Hooks
; kb-minor-mode-hook
;   "*Hook called when temp-buffer-manager minor mode is activated or deactivated."

; kb-minor-mode-timer-hook
;   "*Hook called when temp-buffer-manager minor mode timer is started."
; Required thirty party packages
; ido-mode
; Set defalut buffer selection regex
; Currently default temporary buffer selection regex is '.+'.

; Manage Buffer exceptions
; To add or delete buffer exceptions in order to prevent some buffers to kill.

; kb-add-current-buffer-to-exception
;     Current default buffer will be added to the exception list.
; kb-remove-current-buffer-from-exception
;     Current default buffer will be deleted from exception list.
; kb-exception-add 
;     Select any existing buffers from the choices given to add to the exception list. 
; kb-exception-delete
;     Select any buffer added to the exception list to remove.
; Manage Major-Mode exceptions
; Add or delete major-modes to the exception list to prevent those modes to be killed.

; kb-mode-exception-add
; kb-mode-exception-delete
; Manage buffer state
; Considering below buffer states before killing.

; 1. whether buffer has modified content
;     By default the modified buffer will not be deleted unless kb-kill-if-modified is set.
; 2. Whether an active process in running on the buffer
;     if kb-prompt-for-running-process is set, user will be prompted to decide whether the buffer should be deleted.
; 3. Whether the buffer is an indirect buffer (not implemented yet)
; Manage Timer
; Set the interval(kb-clean-interval) to clean temporary buffers and start the timer.

; To start the timer:
;     M-x kb-start-timer
; To stop the timer:
;     M-x kb-stop-timer
; Kb-help
; kb-help function let you know the current conditions used to kill temporary buffer. Ex:

; Temporary buffer management:

; Conditions used to kill temp buffers:
; Buffer exception list:
;         *Messages*, *scratch*

; Buffer selection default regex:
;         \*.+\*
;         Buffer selection exception regex:
;         nil

; Buffer mode exceptions:
;     nil

; Buffer state conditions:
;         Buffers will not be killed if it has modified content.(To customize: kb-kill-if-modified)
;     Buffers with running process will be killed without prompting.(To customize: kb-prompt-for-running-process)

; Kill buffers periodically:
;         To start the timer M-x kb-start-timer and the temp buffers will be killed for each 10 seconds

;     To stop the timer M-x kb-stop-timer.

(require 'ido)

;;; Code:

;; customization stuff
(defgroup kb nil
  "Kill temporary buffers."
  :group 'tools
  :prefix "kb-"
  :version "1.0"
  :group 'convenience)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom kb-minor-mode-hook nil
  "*Hook called when temp-buffer-manager minor mode is activated or deactivated."
  :type 'hook
  :group 'kb)

(defcustom kb-minor-mode-timer-hook nil
  "*Hook called when temp-buffer-manager minor mode timer is started or stopped."
  :type 'hook
  :group 'kb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor mode variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kb-exception-alist  '("*Messages*" "*scratch*")
  "List of buffers should not be deleted.")

(defvar kb-regex-alist '()
  "Regex to match temporary buffers.")

(defvar kb-mode-exceptions-list '()
  "List of modes that should not be deleted.")

(defvar kb-buffer-state-list '()
  "List of buffer states that can be deleted.")

(defvar kb-help-buffer "*kb-help*"
  "Buffer to show temp buffer management help.")

(defvar kb-clean-timer nil
  "Timer for cleaning buffers periodically.")

(defvar kb-mode-line-lighter " kb"
  "Mode line string for kb minor mode.")

(defvar-local kb-mode-line kb-mode-line-lighter
  "The mode line lighter of variable `kb-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customizable variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom kb-default-temp-buffer-regex "\\*.+\\*"
  "Default regex to match temp buffers."
  :type 'string
  :group 'kb)

(defcustom kb-kill-indirect-buffers nil
  "Flag to indicate whether or not to kill indirect buffers."
  :type 'boolean
  :group 'kb)

(defcustom kb-kill-if-modified nil
  "Flag to indicate whether or not to kill the buffer if it has modifed content."
  :type 'boolean
  :group 'kb)

(defcustom kb-prompt-for-running-process nil
  "Flag to indicate whether or not to prompt user when killing a buffer in which a process run."
  :type 'boolean
  :group 'kb)

(defcustom kb-kill-anyway nil 
  "Flag to indicate whether or not kill the buffers regardless of its state such as modified, indirect buffer etc."
  :type 'boolean
  :group 'kb)

(defcustom kb-clean-periodically t
  "Flag to indicate whether or not to run temp buffer cleaning periodically."
  :type 'boolean
  :group 'kb)

(defcustom kb-clean-interval 10
  "Time interval to run temp buffer cleaning."
  :type 'number
  :group 'kb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the keymap.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kb-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c k s") 'kb-start-timer)
    (define-key map (kbd "C-c k S") 'kb-stop-timer)
    (define-key map (kbd "C-c k k") 'kb-kill)
    (define-key map (kbd "C-c k c a") 'kb-add-current-buffer-to-exception)
    (define-key map (kbd "C-c k c d") 'kb-remove-current-buffer-from-exception)
    (define-key map (kbd "C-c k r a") 'kb-add-selection-regex)
    (define-key map (kbd "C-c k r d") 'kb-delete-selection-regex)
    (define-key map (kbd "C-c k e a") 'kb-exception-add)
    (define-key map (kbd "C-c k e d") 'kb-exception-delete)
    (define-key map (kbd "C-c k m a") 'kb-mode-exception-add)
    (define-key map (kbd "C-c k m d") 'kb-mode-exception-delete)
    (define-key map (kbd "C-c k h") 'kb-help)
    map)
  "A keymap for temp-buffer-manager minor mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer exception manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-add-current-buffer-to-exception ()
  "Interactive function to add current buffer to add in exception list."
  (interactive)
  (kb-add-exception-internal (buffer-name)))

(defun kb-remove-current-buffer-from-exception ()
  "Interactive function to remove current buffer in exception list if its added."
  (interactive)
  (let ((buffername (buffer-name)))
    (if buffername
	(progn
	  (setq kb-exception-alist (delq buffername kb-exception-alist))
	  (kb-util-message (concat "Buffer " buffername " removed from kb exception list."))))))

(defun kb-add-exception-internal (buffername)
  "Internal function to add given BUFFERNAME to exception list."
  (cond
   ((not buffername) (message "No buffer to add."))
   ((if (member buffername kb-exception-alist)
	(kb-util-message (concat "Buffer " buffername " already exists in exception list."))
      (progn
	(setq kb-exception-alist (cons buffername kb-exception-alist))
	(kb-util-message (concat "Buffer " buffername " added to kb exception list.")))))))

(defun kb-exception-add ()
  "Interactive function to add given buffer to exception list."
  (interactive)
  (let ((buffer-selected (ido-read-buffer "Add kb exception: " nil t))) ;; calling ido-read-buffer with require-match set
    (if buffer-selected
	(kb-add-exception-internal buffer-selected))))

(defun kb-exception-delete ()
  "Interactive function to delete selected buffer from exception list."
  (interactive)
  (let ((buffer-to-delete (ido-completing-read "Remove kb exception: " kb-exception-alist nil t nil nil nil nil)))
    (if buffer-to-delete
	(progn
	  (setq kb-exception-alist (kb-util-delete-from-list buffer-to-delete kb-exception-alist))
	  (kb-util-message (concat "Buffer " buffer-to-delete " is removed from exception list."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major mode exception manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-mode-exception-add ()
  "Adding given mode to the mode exception list."
  (interactive)
  (let ((mode (read-from-minibuffer "Mode: " nil nil nil 'kb-mode-exceptions-list)))
    (if mode
	(progn
	  (setq kb-mode-exceptions-list (kb-util-add-no-dup mode kb-mode-exceptions-list))
	  (kb-util-message (concat "Mode " mode " added in exception list."))))))

(defun kb-mode-exception-delete ()
  "Deleting seleted mode from exception list."
  (interactive)
  (let ((mode-to-delete (ido-completing-read "Mode to delete: " kb-mode-exceptions-list nil t nil nil nil nil)))
    (if mode-to-delete
	(progn
	  (setq kb-mode-exceptions-list (kb-util-delete-from-list mode-to-delete kb-mode-exceptions-list))
	  (kb-util-message (concat "Mode " mode-to-delete " is removed from exception list."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer state manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-running-process-p (buffername)
  "Check whether a process is running on given BUFFERNAME."
  (if (and buffername (get-buffer-process buffername))
      t 
    nil))

(defun kb-buffer-modified-p (buffername)
  "Return whether or not the given BUFFERNAME is modified."
  (if (and buffername (buffer-modified-p (get-buffer buffername)))
      t
    nil))

;; TODO: include indirect buffer check
;; (defun kb-buffer-indirect-p (buffername)
;;   "Check whether given BUFFERNAME is indirect buffer or not."
;; )

(defun kb-state-check (buffername)
  "Return true if the given BUFFERNAME can be killed by checking its state and custom variables."
  (if buffername
      (let ((is-modified (kb-buffer-modified-p buffername))
	    (is-process-running (kb-running-process-p buffername))
	    (can-delete nil))
	(if (not kb-kill-anyway)
	    (if (or (or (and
			  (not is-modified)
			  (not is-process-running))
			(and
			 (and kb-prompt-for-running-process 
			      is-process-running)
			 (kb-util-prompt-to-kill (concat "Process is running on " buffername))))
		    (and kb-kill-if-modified 
			 is-modified))
		(setq can-delete t))
	  (setq can-delete t)) can-delete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exception regex manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-add-selection-regex ()
  "Interactive function to add temporary buffer selection regex."
  (interactive)
  (let ((regex (read-from-minibuffer "Regex: " nil nil nil 'kb-regex-alist)))
    (kb-util-add-no-dup regex kb-regex-alist)))

(defun kb-delete-selection-regex ()
  "Interactive function to delete temporary buffer selection regex."
  (interactive)
  (let ((regex-to-delete (ido-completing-read "Remove regex: " kb-regex-alist nil t nil nil nil nil)))
    (if regex-to-delete
	(progn
	  (setq kb-regex-alist (kb-util-delete-from-list regex-to-delete kb-exception-alist))
	  (kb-util-message (concat "Regex " regex-to-delete " is removed from selection regex list."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-util-show-list (list-to-print)
  "Printing LIST-TO-PRINT."
  (let ((current-list list-to-print)
	(list-to-str (if (= 0 (length list-to-print)) "nil" "")))
    (while current-list
      (setq list-to-str (concat list-to-str (car current-list)))
      (setq current-list (cdr current-list))
      (if current-list
	  (setq list-to-str (concat list-to-str ", ")))) (concat list-to-str "\n\n")))

(defun kb-util-delete-from-list (element list)
  "Util function to delete given ELEMENT from given LIST."
  (if (and element list)
      (setq list (delq element list))) list)

(defun kb-util-add-no-dup (element list)
  "Util function to add given ELEMENT to LIST without duplicates."
  (if (and (and element list)
	   (not (member element list)))
      (setq list (kb-util-add-to-list element list))) list)

(defun kb-util-prompt-to-kill (promptmsg)
  "Prompting user with PROMPTMSG to kill some buffers."
  (yes-or-no-p promptmsg))

(defun kb-util-add-to-list (element list)
  "Util function to add given ELEMENT to given LIST."
  (if (and element list)
      (setq list (cons element list))) list)

(defun kb-util-message (msg)
  "Function to show kb messages(MSG)."
  (message msg))

(defun kb-util-get-major-mode (buffername)
  "Return major mode of the given BUFFERNAME."
  (with-current-buffer buffername
    (symbol-name major-mode)))

(defun kb-util-buffer-regex-check (buffername)
  "Check whether given BUFFERNAME is matching with the selection regex list."
  (let ((regex-list kb-regex-alist)
	(deletable nil))
    (if (= 0 (length regex-list))
	(setq deletable t)
      (progn
	(while (and regex-list (not deletable))
	  (if (string-match (car regex-list) buffername)
	      (setq deletable t))
	  (setq regex-list (cdr regex-list))))) deletable))

(defun kb-util-buffer-mode-check (buffername)
  "Check whether given BUFFERNAME's mode is in mode exception list."
  (let ((mode-list kb-mode-exceptions-list)
	(buffer-major-mode (kb-util-get-major-mode buffername))
	(deletable nil))
    (if (or (not buffer-major-mode) (= 0 (length mode-list)))
	(setq deletable t)
      (progn
	(while (and mode-list (not deletable))
	  (if (string-equal (car mode-list) buffer-major-mode)
	      (setq deletable t))
	  (setq mode-list (cdr mode-list))))) deletable))

(defun kb-util-set-mode-line (str)
  "Set mode line for temp buffer manager minor mode as STR."
  (unless str
    (setq str ""))
  (setq kb-mode-line str)
  (force-mode-line-update))

(defun kb-default-temp-buffer-check (buffername)
  "Check whether the given BUFFERNAME match with default temp buffer regex."
  (if (and (and 
	    buffername 
	    kb-default-temp-buffer-regex) 
	   (string-match kb-default-temp-buffer-regex buffername))
      t
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kb kill functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-kill ()
  "Kill all temporary buffers.
Match buffers with regex `kb-default-temp-buffer-regex except below conditions.
buffer exception list: list of buffer names which should not be deleted
buffer selection regex: list of regex to exclude buffers
buffer mode exceptions: list of major modes to exclude buffers
buffers with certain states: buffer modified, process running in the buffer etc.,"
  (interactive)
  (let* ((list-of-buffers (mapcar (function buffer-name) (buffer-list)))
	 (current-buffer-name nil))
    (kb-util-set-mode-line (concat kb-mode-line-lighter "*"))
    (while list-of-buffers
      (setq current-buffer-name (car list-of-buffers))
      (when (and (kb-default-temp-buffer-check current-buffer-name)
		 (and (not (member current-buffer-name kb-exception-alist))
		      (and
		       (kb-state-check current-buffer-name)
		       (and (kb-util-buffer-mode-check current-buffer-name)
			    (kb-util-buffer-regex-check current-buffer-name)))))
	(progn
	  (kb-util-message (concat "Killing buffer: " current-buffer-name))
	  (kill-buffer current-buffer-name)))
      (setq list-of-buffers (cdr list-of-buffers)))
    (kb-util-message "Killed all temporary buffers.")
    (kb-util-set-mode-line (concat kb-mode-line-lighter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-start-timer ()
  "Interactive function to start clean buffer timer."
  (interactive)
  (if (not kb-clean-timer)
      (setq kb-clean-timer (run-at-time t kb-clean-interval 'kb-kill))
    (progn
      (cancel-timer kb-clean-timer)
      (setq kb-clean-timer (run-at-time t kb-clean-interval 'kb-kill))))
  (kb-util-set-mode-line (concat kb-mode-line-lighter "*")))

(defun kb-stop-timer ()
  "Interactive function to stop the clean timer if its already running."
  (interactive)
  (if kb-clean-timer
      (cancel-timer kb-clean-timer)
    (kb-util-message "Clean timer doesn't exits."))
  (kb-util-set-mode-line kb-mode-line-lighter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kb help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kb-get-state-conditions ()
  "Printing state check conditions."
  (let ((msg-str ""))
    (if kb-kill-anyway
	(setq msg-str "\tBuffers will be killed regardless of its state.(To customize: kb-kill-anyway)")
      (progn
	(if kb-kill-if-modified
	    (setq msg-str "\tBuffers will be killed though its modified.(To customize: kb-kill-if-modified)\n")
	  (setq msg-str "\tBuffers will not be killed if it has modified content.(To customize: kb-kill-if-modified)\n"))
	(if kb-prompt-for-running-process
	    (setq msg-str (concat msg-str "\tPrompting user when killing buffer in which a process run.(To customize: kb-prompt-for-running-process)\n"))
	  (setq msg-str (concat msg-str "\tBuffers with running process will be killed without prompting.(To customize: kb-prompt-for-running-process)")))))
    (concat msg-str "\n\n")))

(defun kb-timer-help ()
  "Returns help string regarding kb timer."
  (let ((str "Kill buffers periodically:"))
    (if kb-clean-timer
	(setq str (concat str "\n\tTimer is already running and buffer are being killed for each " (int-to-string kb-clean-interval) " seconds."))
      (setq str (concat str "\n\tTo start the timer M-x kb-start-timer and the temp buffers will be killed for each " (int-to-string kb-clean-interval) " seconds")))
    (setq str (concat str "\n\n\tTo stop the timer M-x kb-stop-timer.")) str))

(defun kb-help ()
  "Displays current conditions for temp buffer management to user in a buffer."
  (interactive)
  (let ((help-buffer (get-buffer-create kb-help-buffer)))
    (switch-to-buffer help-buffer)
    (insert (concat "Temporary buffer management:\n\nConditions used to kill temp buffers:\n"
			   "Buffer exception list:\n\t"
			   (kb-util-show-list kb-exception-alist)
			   "Buffer selection default regex:\n\t"
			   kb-default-temp-buffer-regex "\n\t"
			   "Buffer selection exception regex:\n\t"
			   (kb-util-show-list kb-regex-alist)
			   "Buffer mode exceptions:\n\t"
			   (kb-util-show-list kb-mode-exceptions-list)
			   "Buffer state conditions:\n"
			   (kb-get-state-conditions)
			   (kb-timer-help)
			   ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode kb-minor-mode
  "Minor mode for managing temp buffers."
  :lighter kb-mode-line
  :keymap kb-minor-mode-map
  (progn
   (run-hooks kb-minor-mode-hook)
   (if kb-clean-timer
       (run-hooks kb-minor-mode-timer-hook))))

(provide 'kb-mode)
;;; kb-mode.el ends here
