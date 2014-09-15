;; eclim-workspace.el --- Customized version of eclim
;;
;; Copyright (C) 2014  Ramesh Kandasamy
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Eclim-workspace enables to open eclipse workspaces from emacs.
;;  - Lists recent workspaces used in eclipse and allow you to choose.
;;  - Starts eclimd server.
;;  - Runs hook once eclimd server starts.
;;     For ex: after starting eclimd, can set project-explorer with workspace.
;;
;;; Conventions
;;
;;* Eclim Project

;;; Code:
(require 'ido)
(require 'eclim)
(require 'eclimd)

(defcustom eclim-ws-multple-instance t
  "Flag to open multiple instance of a workspace."
  :group 'eclim
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom eclim-ws-eclimd-start-hook nil
  "Hook to run when eclimd start."
  :group 'eclim
  :type 'hook)

(defvar eclim-workspace-dir nil
  "Current workspace directory.")

(defvar eclim-ws-parent-dir nil
  "Root directory of all the workspaces.")

(defvar eclim-ws-eclipse-config-dir nil
  "Eclipse local configuration directory.")

(defvar eclim-ws-eclipse-preferences "/.settings/org.eclipse.ui.ide.prefs"
  "Eclipse preferences file to get recent workspaces.")

(defun eclim-ws-get-src (workspace)
  "Get src directory for given WORKSPACE."
    (if workspace
	(concat (replace-regexp-in-string "\/$" "" workspace) "/src/")
      nil))

(defun eclim-ws-get-recent-workspaces ()
  "Return recent workspaces in a list."
  (let ((pref-config (concat (concat eclim-ws-eclipse-config-dir eclim-ws-eclipse-preferences)))
	(eclipse-prefs nil)
	(recent-ws nil)
	(prefs-recent-ws-regexp "RECENT_WORKSPACES=\\(.+\\)\\\\n"))
    (if (file-exists-p pref-config)
	(progn
	  (with-temp-buffer
	    (insert-file-contents pref-config)
	    (setq eclipse-prefs (buffer-substring-no-properties (point-min) (point-max)))))
      (message "Eclipse workspace preferences file doesn't exists."))
    (if (and eclipse-prefs (string-match prefs-recent-ws-regexp eclipse-prefs))
	(progn
	  (setq recent-ws (match-string 1 eclipse-prefs))
	  (setq recent-ws (split-string recent-ws "\\\\n")))
      nil) recent-ws))

(defun eclim-ws-list ()
  "Listing the workspaces."
  (let ((recent-ws (eclim-ws-get-recent-workspaces)))
    (if recent-ws
	(add-to-list 'recent-ws "*Browse*" t)
      (directory-files eclim-ws-parent-dir nil "[^\.]"))))

(defun wait-eclimd-start ()
  "Wait for the eclimd server to become active.
This function also waits for the eclimd server to report that it is started.
It returns the port it is listening on"
  (let ((eclimd-start-regexp "Eclim Server Started on\\(?: port\\|:\\) \\(?:\\(?:[0-9]+\\.\\)\\{3\\}[0-9]+:\\)?\\([0-9]+\\)")
	(eclimd-port nil))
    (save-match-data
      (let ((output (eclimd--match-process-output eclimd-start-regexp eclimd-process)))
	(when output
	  (setq eclimd-port (match-string 1 output))
	  (message (concat "eclimd serving at port " eclimd-port))
	  (run-hooks 'eclim-ws-eclimd-start-hook))))
    eclimd-port))

(defun eclim-ws-start-eclimd (workspace-dir)
  "Starting eclimd for WORKSPACE-DIR."
  (let ((eclimd-prog (eclimd--executable-path)))
    (if (not eclimd-prog)
        (message "Cannot start eclimd: check eclimd-executable variable.")
      (if (eclimd--running-p)
          (message "Cannot start eclimd: eclimd is already running.")
        (message (concat "Starting eclimd for workspace: " workspace-dir "..."))
	(setq eclimd-current-workspace workspace-dir)
        (setq eclimd-process-buffer
              (make-comint eclimd-process-buffer-name
                           eclimd-prog
                           nil
                           (concat "-Dosgi.instance.area.default="
                                   (replace-regexp-in-string "~" "@user.home" workspace-dir))))
        (setq eclimd-process (get-buffer-process eclimd-process-buffer))
        (when eclimd-wait-for-process
          (wait-eclimd-start))))))

(defun eclim-ws-remove-lock (workspace)
  "Remove the lock for WORKSPACE to open multiple instances."
  (if workspace
      (progn
	(message "Removing %s workspace lock." workspace)
	(shell-command-to-string (concat "rm -f " workspace "/.metadata/.lock")))))

(defun eclim-ws-open ()
  "Open a workspace by starting eclimd for the same and update speed-bar."
  (interactive)
  (let ((workspace-dir (ido-completing-read "Workspace: " (eclim-ws-list) nil t nil nil nil nil)))
    (if (string-equal workspace-dir "*Browse*")
	(setq workspace-dir (read-directory-name "Workspace directory: " eclimd-default-workspace nil t)))
    (cond
     ((unless workspace-dir (message "Cannot open workspace: null")))
     (t (progn
       (if eclim-ws-multple-instance
	   (eclim-ws-remove-lock workspace-dir))
       (setq eclim-workspace-dir workspace-dir)
       (eclim-ws-start-eclimd workspace-dir)
       )))))

(provide 'eclim-workspace)
;;; eclim-workspace.el ends here
