;;; turing-machine --- turing-machine game for emacs
;;; Author: Ramesh Kandasamy <krameshkrr@gmail.com>
;;; Created: July 2014
;;; Version: 1.0
;;; Keywords: game

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
;;
;; Turing machine:
;;   Computing machine that can use a predefined set of rules to determine
;; a result from a set of input variables.
;;

;;; Install:
;;  clone it from github https://github.com/krameshkrr/Emacs/tree/master/turing-machine/
;;  Add the turing machine packages path to load path and Add below line in emacs init file
;;  
;;  (require 'turing-machine)
;;
;; To start the puzzle game:
;;  M-x turing-machine

;;
;; Note: The buffer is read only and don't try to make it writable. It may behave unexpected.
;;

;;; Key bindings:
;; - Use <left> and <right> keys to move across the states which can you control.
;; - Use <space> to change the value of the state
;; - 'p' to play the states and match the tape with targets
;; - 'R' to reset the puzzle
;; - 'q' to quit the game.
;; - M-x tm-easy-mode to play only easy puzzles.
;; - M-x tm-moderate-mode to play moderate level puzzles.
;; - M-x tm-difficult-mode to play complex puzzles.


;;; Code:

(require 'tm-puzzles)
(require 'tm-board)
(require 'artist)

(defgroup turing-machine nil
  "turing-machine - puzzle game."
  :group  'games
  :prefix "tm-")

(defcustom tm-animate-delay .5
  "Delay in seconds when animating state."
  :type  'number
  :group 'turing-machine)

(defcustom tm-mode-hook nil
  "Hook run on starting turing machine."
  :type  'hook
  :group 'turing-machine)

(defcustom tm-exit-hook nil
  "Hook run on exiting turing machine."
  :type 'hook
  :group 'turing-machine)

(defvar artist-picture-compatibility-backup nil
  "Store previous value of picture compatibility.")

(defvar tm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?"                       #'describe-mode)
    (define-key map " "                       #'tm-toggle-state-value)
    (define-key map "p"                       #'tm-play)
    (define-key map "R"                       #'tm-reset-puzzle)
    (define-key map [left]                    #'tm-move-previous)
    (define-key map [right]                   #'tm-move-next)
    (define-key map [(control e)]             #'tm-easy-mode)
    (define-key map [(control m)]             #'tm-moderate-mode)
    (define-key map [(control d)]             #'tm-difficult-mode)
    (define-key map "q"                       #'tm-quit)
    map)
  "Local keymap for the turing machine game.")

(defun tm-process-tape-value (value)
  "Set tape value to given VALUE if it differs."
  (let* ((inhibit-read-only t)
	 (t-current (elt tm-tape-vector tm-tape-vector-index))
	 (print-val nil)
	 (current-val (car t-current)))
    (if (string= current-val " ")
	(setq current-val "-1"))
    (if (not (= value (string-to-number current-val)))
	(cond ((< value 0)
	       (setq print-val " "))
	      ((= value 0)
	       (setq print-val "0"))
	      (t (setq print-val "1"))))
    (if (stringp print-val)
	(progn
	  (aset tm-tape-vector tm-tape-vector-index (list print-val (cadr t-current) (caddr t-current)))
	  (artist-mode)
	  (artist-text-insert-common (+ 2 (cadr t-current))
				     (+ (/ tm-tape-width 2) (cadr (cdr t-current)))
				     print-val
				     nil)
	  (tm-artist-mode-off)
	  ))))

(defun tm-check-conditional (cond-val)
  "Check conditional state value against the current tape value."
  (let ((current-val (car (elt tm-tape-vector tm-tape-vector-index))))
    (if (and (string= current-val " ") (< cond-val 0))
	t
      (if (= (string-to-number current-val) cond-val)
	  t nil))))

(defun tm-check-loop-condition (cond-val)
  "Check conditional state value against the current tape value."
  (let ((current-val (car (elt tm-tape-vector tm-tape-vector-index))))
    (if (or (= cond-val -2) ;; infinite loop
	    (and (string= current-val " ") (< cond-val 0))
	    (= (string-to-number current-val) cond-val))
	t nil)))

(defun tm-move-tape-cursor (direction)
  "Move the tape cursor for given DIRECTION."
  (let* ((inhibit-read-only t)
	 (t-current (elt tm-tape-vector tm-tape-vector-index))
	 (clear-current nil)
	 (x-pos (cadr t-current))
	 (y-pos (caddr t-current)))
    (artist-mode)
    (cond ((and (= direction 0) (> tm-tape-vector-index 0)) 
	   (progn
	     (artist-text-insert-common (- x-pos 2)
					(+ 1 (/ tm-tape-width 2) y-pos)
					"_"
					nil)
	     (setq tm-tape-vector-index (- tm-tape-vector-index 1))
	     (setq clear-current t)))
	  ((and (= direction 1) (< tm-tape-vector-index (- (length tm-tape-vector) 1)))
	   (progn
	     (artist-text-insert-common (+ x-pos 6)
					(+ 1 (/ tm-tape-width 2) y-pos)
					"_"
					nil)
	     (setq tm-tape-vector-index (+ tm-tape-vector-index 1))
	     (setq clear-current t))))
    (if clear-current
	;; Clear current cursor pointer
	(artist-text-insert-common (+ x-pos 2)
				   (+ 1 (/ tm-tape-width 2) y-pos)
				   " "
				   nil))
    (tm-artist-mode-off)))

(defun tm-unset-tape-cursor ()
  "Unsetting tape cursor point."
  (let* ((inhibit-read-only t)
	 (t-current (elt tm-tape-vector tm-tape-vector-index))
	 (x-pos (cadr t-current))
	 (y-pos (caddr t-current)))
    (artist-mode)
    (artist-text-insert-common (+ x-pos 2)
			       (+ 1 (/ tm-tape-width 2) y-pos)
			       " "
			       nil)
    (tm-artist-mode-off)))

(defun tm-move-state (set-index state-index direction &optional iteration)
  "Moving to next state given by SET-INDEX STATE-INDEX and the DIRECTION."
  (let ((next-set set-index)
	(current-state-list nil)
	(next-state-list state-index)
	(return-list t)
	(next-state-index state-index)
	(next-state-vector nil)
	(state-vector nil))
    (if (not iteration)
	(setq iteration 1))
    (if (= set-index 0)
	(setq state-vector (car tm-state-values))
      (setq state-vector (cadr tm-state-values)))
    (while (and (> iteration 0) return-list)
      (cond ((= direction 0)
	     (setq next-state-index (- state-index 1)))
	    ((= direction 1)
	     (setq next-state-index (+ state-index 1)))
	    ((= direction -1)
	     (setq next-set 0))
	    ((= direction -2)
	     (setq next-set 1)))
      (if (not next-state-vector)
	  (if (= next-set 0)
	      (setq next-state-vector (car tm-state-values))
	    (setq next-state-vector (cadr tm-state-values))))
      
      (setq current-state-list (elt state-vector state-index))
    
      (if (and (>= next-state-index 0) (< next-state-index (length state-vector)))
	  (progn
	    (setq next-state-list (elt next-state-vector next-state-index))
	    (tm-unset-state-cursor (cdr current-state-list))
	    (tm-set-state-cursor (cdr next-state-list))
	    (setq state-index next-state-index)
	    (setq return-list (list next-state-list next-set next-state-index)))
	(setq return-list nil))
      (setq iteration (- iteration 1))
      ) return-list))

(defun tm-set-state-cursor (state-positions)
  "Setting the cursor point given in STATE-POSITIONS in turing machine states."
  (if state-positions
      (let ((inhibit-read-only t)
	    (x-pos (car state-positions))
	    (y-pos (cadr state-positions)))
	(artist-mode)
	(artist-text-insert-common (+ x-pos (/ tm-state-length 2))
				   (+ y-pos (- tm-state-width 1))
				   "_" nil)
	(tm-artist-mode-off))))

(defun tm-unset-state-cursor (state-positions)
  "Unsetting the cursor point given in STATE-POSITIONS in turing machine states."
  (if state-positions
      (let ((inhibit-read-only t)
	    (x-pos (car state-positions))
	    (y-pos (cadr state-positions)))
	(artist-mode)
	(artist-text-insert-common (+ x-pos (/ tm-state-length 2))
				   (+ y-pos (- tm-state-width 1))
				   " " nil)
	(tm-artist-mode-off)
	)))

(defun tm-play ()
  "Play from initial state of game `turing-machine'."
  (interactive)
  (let ((c-state-list (elt (car tm-state-values) 0))
	(c-state nil)
	(set-index 0)
	(loop-direction nil)
	(next-state nil)
	(processed nil)
	(state-index 0))
    (setq c-state (car c-state-list))
    (setq cursor-type nil)
    (if c-state
	(progn
	  (tm-load-message "Processing states...")
	  (tm-set-state-cursor (cdr c-state-list))
	  (while (not processed)
	    (sit-for tm-animate-delay)
	    (cond
	     ((tm-empty-p c-state) (setq next-state (tm-move-state set-index
								 state-index
								 1)))
	     ((tm-state-p c-state) (progn
				     (tm-process-tape-value (tm-state-value c-state))
				     (setq next-state (tm-move-state set-index
								     state-index
								     1))))
	     ((tm-direction-p c-state) (progn 
					 (tm-move-tape-cursor (tm-direction-value c-state))
					 (setq next-state (tm-move-state set-index
									 state-index
									 1)) ;; always move to right
					 ))
	     ((tm-conditional-state-p c-state) (progn
						 (if (tm-check-conditional (tm-conditional-state-value c-state))
						     (setq next-state (tm-move-state set-index
										     state-index
										     (tm-conditional-state-direction c-state)))
						   (setq next-state (tm-move-state set-index
										   state-index
										   1)))))
	     ((tm-loop-p c-state) (progn
				    (if (tm-check-loop-condition (tm-loop-value c-state))
					(progn
					  (setq loop-direction (if (< (tm-loop-end c-state) 0) 0 1))
					  (setq next-state (tm-move-state set-index
									  state-index
									  loop-direction
									  (abs (tm-loop-end c-state))
									  )))
				      (setq next-state (tm-move-state set-index
								      state-index
								      1))
				      )
				    ))
	     )
	    (if (not next-state)
		(setq processed t)
	      (progn
		(setq c-state-list (car next-state)
		      set-index (cadr next-state)
		      state-index (caddr next-state))
		(setq c-state (car c-state-list))))
	    )
	  (tm-unset-state-cursor (cdr c-state-list))
	  (tm-unset-tape-cursor)
	  (tm-load-message "Processing states. Done..")
	  (sit-for tm-animate-delay)
	  (tm-load-message "Matching with target..")
	  (sit-for tm-animate-delay)
	  (if (not (tm-check))
	      (progn
		(tm-load-message "Target failed to match.. Resetting puzzle..")
		(sit-for tm-animate-delay)
		(tm-reset-puzzle)
		)
	    (progn
	      (tm-load-message "Target Matched..")
	      (ding t)
	      (if (yes-or-no-p "Want to load next puzzle?")
		  (tm-next-puzzle)
		(progn
		  (tm-load-message "Thank you for playing.. exiting.")
		  (sit-for tm-animate-delay)
		  (tm-quit)))
	    ))
	  ))
    (setq cursor-type t)
    (setq tm-state-cursor 0)
    (tm-position-cursor)))

(defun tm-check ()
  "Checking turing machine tape content against the target.
Return t if it matches else nil."
  (let ((target-list (mapcar 'string (tm-puzzle-target tm-current-puzzle)))
	(tape-iterator 0)
	(failed nil)
	(tape-length (length tm-tape-vector)))
    (while (and (< tape-iterator tape-length) (string= (car (elt tm-tape-vector tape-iterator)) " "))
      (setq tape-iterator (+ 1 tape-iterator)))
    (while (and (not failed) target-list (< tape-iterator tape-length))
      (if (not (string= (car target-list) (car (elt tm-tape-vector tape-iterator))))
	  (setq failed t))
      (setq target-list (cdr target-list))
      (setq tape-iterator (+ 1 tape-iterator)))

    (if (not failed)
	(while (< tape-iterator tape-length)
	  (if (not (string= (car (elt tm-tape-vector tape-iterator)) " "))
	      (setq failed t))
	  (setq tape-iterator (+ 1 tape-iterator)))
	)
    (if (not failed)
	t nil)))

(defun tm-util-get-next-in-list (val list)
  "Get next available value for given VAL to load from allowed LIST."
  (let ((next-val nil)
	(found nil)
	(first-val nil))
  (if list
      (progn
	(setq first-val (car list))
	(while (and list (not found))
	  (if (= (car list) val)
	      (setq found t)
	    (setq list (cdr list))))
	(if found
	    (if (not (cdr list))
		(setq next-val first-val)
	      (setq next-val (cadr list))))))
  next-val))

(defun tm-get-state-next-value (state)
  "Return next value for given variable STATE."
  (let ((next-val nil)
	 (current-val (tm-get-state-value state))
	 (allowed-list (tm-get-state-allowed-list state)))
    (if allowed-list
	(setq next-val (tm-util-get-next-in-list current-val allowed-list)))
    next-val))

(defun tm-toggle-state-value ()
  "Toggle next available values of the current state of game `turing-machine'."
  (interactive)
  (let ((state-xy nil)
	(state-index nil)
	(index nil)
	(next-print-val nil)
	(next-val nil)
	(state nil))
    (if (and tm-state-values 
	     (> tm-state-cursor -1))
	(progn
	  (setq state-xy (elt tm-variable-state-positions tm-state-cursor))
	  (setq state-index (caddr (elt tm-variable-state-positions tm-state-cursor))) ;; caddr

	  (if state-index
	      (progn
		(setq index (cadr state-index))
		(if (= (car state-index) 1)
		    (setq state (car (elt (cadr tm-state-values) index)))
		  (setq state (car (elt (car tm-state-values) index))))

		(setq next-val (tm-get-state-next-value state))

		(tm-set-state-value next-val state)
		(setq next-print-val (tm-get-state-print-value state))
		(tm-load-value next-print-val (car state-xy) (cadr state-xy))
		))))))

(defun tm-move-previous ()
  "Move to previous variable state of game `turing-machine'."
  (interactive)
  (let ((tm-state-cursor-xy nil)
	(tm-varaibles-length (length tm-variable-state-positions)))
    (if (and tm-varaibles-length
	     tm-state-cursor 
	     (> tm-state-cursor -1))
	(progn
	  (setq tm-state-cursor (- tm-state-cursor 1))
	  (if (< tm-state-cursor 0)
	      (setq tm-state-cursor (- (length tm-variable-state-positions) 1)))
	  (setq tm-state-cursor-xy (elt tm-variable-state-positions tm-state-cursor))
	  (artist-mode)
	  (artist-move-to-xy (car tm-state-cursor-xy) (cadr tm-state-cursor-xy))
	  (tm-artist-mode-off)))))

(defun tm-move-next ()
  "Move cursor to next variable state of game `turing-machine'."
  (interactive)
  (let ((tm-state-cursor-xy nil)
	(tm-varaibles-length (length tm-variable-state-positions)))
    (if (and tm-varaibles-length
	     tm-state-cursor (> tm-state-cursor -1))
	(progn
	  (setq tm-state-cursor (+ 1 tm-state-cursor))
	  (if (>= tm-state-cursor (length tm-variable-state-positions))
	      (setq tm-state-cursor 0))
	  (setq tm-state-cursor-xy (elt tm-variable-state-positions tm-state-cursor))
	  (artist-mode)
	  (artist-move-to-xy (car tm-state-cursor-xy) (cadr tm-state-cursor-xy))
	  (tm-artist-mode-off)))))

(defun tm-easy-mode ()
  "Easy mode of game `turing-machine'."
  (interactive)
  (tm-load-puzzle-difficulty 1))

(defun tm-moderate-mode ()
  "Moderate mode of game `turing-machine'."
  (interactive)
  (tm-load-puzzle-difficulty 2))

(defun tm-difficult-mode ()
  "Difficult mode of `turing-machine'."
  (interactive)
  (tm-load-puzzle-difficulty 3))

(defun tm-load-puzzle-difficulty (difficulty)
  "Loading puzzle for the DIFFICULTY."
  (setq tm-difficulty difficulty)
  (setq tm-puzzle-list (tm-get-puzzle-list tm-difficulty))
  (if tm-puzzle-list
      (progn
	(setq tm-current-puzzle-index 0)
	(tm-init-puzzle (car tm-puzzle-list))
	(tm-load-help)
	(tm-position-cursor))))

(defun tm-reset-puzzle ()
  "Reset the puzzle of game `turing-machine'."
  (interactive)
  (let ((inhibit-read-only t))
    (artist-mode)
    (tm-load-tape tm-current-puzzle-source)
    (tm-load-states (tm-puzzle-states tm-current-puzzle-source))
    (tm-position-cursor)
    (tm-artist-mode-off)))

(defun tm-next-puzzle ()
  "Setting the next puzzle in the difficulty level."
  (let ((c-length (length tm-puzzle-list)))
    (setq tm-current-puzzle-index (+ 1 tm-current-puzzle-index))
    (if (>= tm-current-puzzle-index c-length)
	(tm-load-message "Congrats! you are done with all puzzles...")
      (tm-init-puzzle (nth tm-current-puzzle-index tm-puzzle-list)))))

(defun tm-quit ()
  "Quit the current game of `turing-machine'."
  (interactive)
  (run-mode-hooks 'tm-exit-hook)
  (kill-buffer tm-buffer-name))

;; Making artist picture compatibility as previous
(add-hook 'tm-exit-hook '(lambda() 
			   (setq artist-picture-compatibility artist-picture-compatibility-backup)))

;;;###autoload
(defun turing-machine ()
  "Setting up `turing-machine' game."
  (interactive)
  (tm-buffer-setup)
  (turing-machine-mode)
  (tm-init))

(put 'turing-machine-mode 'mode-class 'special)
(defun turing-machine-mode ()
  "A mode for playing `turing-machine'.

The key bindings for `turing-machine-mode' are:

\\{tm-mode-map}"
  (kill-all-local-variables)
  (use-local-map tm-mode-map)
  (setq major-mode 'turing-machine-mode
        mode-name  "Turing-Machine")
  (run-mode-hooks 'tm-mode-hook)
  (setq buffer-read-only t
        truncate-lines   t)
  (setq artist-picture-compatibility-backup artist-picture-compatibility)
  (setq artist-picture-compatibility nil)
  (buffer-disable-undo))

(provide 'turing-machine)
;;; turing-machine.el ends here
