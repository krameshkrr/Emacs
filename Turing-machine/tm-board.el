;;; tm-board.el --- Load board for turing machine
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
;; Defining the position of each block in turing machine and 
;; load the puzzle game board using artist-mode.
;;

;;; Code:

(eval-when-compile
   (require 'cl))

(require 'artist)
(require 'tm-puzzles)

;; Tape
(defvar tm-tape-length 40
  "The size of the tape.")

(defvar tm-tape-width 3
  "The width of the tape.")

(defvar tm-tape-position-x 5
  "Position of tape.")

(defvar tm-tape-position-y 10
  "Position of tape in y axis.")

(defvar tm-tape-block-size 4
  "Size of each block in tape.")

(defvar tm-tape-empty-states nil
  "Number of empty states can be there before and after the initial value.")

(defvar tm-tape-vector nil
  "Storing turing machine tape content in a vector for easy access.")

(defvar tm-tape-vector-index nil
  "Index of current element of turing machine tape.")

;; Target code block
(defvar tm-target-code-length 10
  "Length of target code.")

(defvar tm-target-length 20
  "Length of target code block.")

(defvar tm-target-width 2
  "Width of target code block.")

(defvar tm-target-block-size 2
  "Size of target block.")

(defvar tm-target-position-x 5
  "Position of target code block.")

(defvar tm-target-position-y 5
  "Position of target code.")

(defvar tm-tape-cursor-position nil
  "Current tape value while processing states.")

;; States
(defvar tm-state-block-length 50
  "Length of the state block.")

(defvar tm-state-block-width 30
  "Width of the state block.")

(defvar tm-state-length 6
  "Size of each state.")

(defvar tm-state-width 3
  "Width of the state.")

(defvar tm-state-interval-horizontal "---"
  "Horizontal State interval.")

(defvar tm-state-interval-vertical "|"
  "Vertical state interval.")

(defvar tm-initial-state-indicator "==>"
  "Initial state indicator.")

(defvar tm-state-position-x 5
  "State x position.")

(defvar tm-state-position-y 17
  "States y position.")

(defvar tm-state-cursor nil
  "Current variable state index.")

(defvar tm-difficulty nil
  "The difficulty level of puzzles to load.")

(defvar tm-current-puzzle-index 0
  "Unique identification for the current puzzle.")

(defvar tm-state-count nil
  "Number of states in each set for a puzzle.")

(defvar tm-state-values nil
  "Values of each states to play and match.")

(defvar tm-current-state-cursor nil
  "Current processing state.")

(defvar tm-variable-state-positions nil
  "Position of each states in which the state values are variable.")

;; buffer
(defvar tm-buffer-name "*Turing-machine*"
  "Name of the turing machine play buffer.")

;; Variables
(defvar tm-current-puzzle nil
  "Modified states of current puzzle.")

(defvar tm-current-puzzle-source nil
  "Original states of current puzzle.")

(defvar tm-puzzle-list nil
  "List of puzzles.")

;; Help block
(defvar tm-help-position-x nil
  "X position of turing maching help block.")

(defvar tm-help-position-y nil
  "Y position of turing machine help block.")

(defvar tm-help-block-length nil
  "Turing machine help block length.")

(defvar tm-help-block-width nil
  "Turing machine help block width.")

(defvar tm-help-content 
  "Turing machine is a computing machine that can
use a predefined set of rules to determine a result
from a set of input variables.

Game:
 Set the states to convert initial tape content to given target.

Symbols:
 '==>' Initial state to start the process.
 '_' points current tape/state to process.
 '*' indicates non-readonly state.
 'n' indicates Null value in state.
 '<-' Moves tape cursor to left.
 '->' Moves tape cursor to right.
 '?' Starting of a loop.
 'oo' Infinite loop & no condition.
 '<value>^/v' Move to up/down if tape value matches with <value>
             else Move right.

Key bindings:
 <space> - To change a non-readonly state value.
 p - To process the states and check tape matches target.
 R - To reset the puzzle.
 q - To quit.
 <left>/<right> - To move the cursor to next non-readonly state.
 Ctrl-e - To play easy level puzzles.
 Ctrl-m - To play moderate level puzzles.
 Crtl-d - To play difficult puzzles.
"
  "Turing machine help content.")

;; Message block
(defvar tm-message-max-length nil
  "Turing machine message block length.")

(defvar tm-message-y-pos nil
  "Turing machine message block y position.")

(defvar tm-message nil
  "Turing machine message.")

;; Buffer setup
(defun tm-buffer-setup ()
  "Setting up buffer for turing machine."
  (get-buffer-create tm-buffer-name)
  (switch-to-buffer tm-buffer-name)
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo))

;; Draw tape and target
(defun tm-get-target-block-length (target-content)
  "Get target block length based on TARGET-CONTENT."
  (let ((content-length (length target-content)))
    (if content-length
	(+ (* 2 content-length) 2)
      tm-target-length)))

(defun tm-get-target-text-to-load (target-content)
  "Return text to print in target tape for given TARGET-CONTENT."
  (let ((target-text nil)
	(iterator-list (mapcar 'string target-content)))
    (while iterator-list
      (setq target-text (concat target-text " " (car iterator-list)))
      (setq iterator-list (cdr iterator-list)))
    (setq target-text (concat target-text " "))
    target-text))

(defun tm-load-target (target-content)
  "Load TARGET-CONTENT at the target position."
  (let ((inhibit-read-only t))
    ;; Print header
    (artist-text-insert-common tm-target-position-x
			       (- tm-target-position-y 1)
			       "Target :" nil)
    ;; Draw rectangle
    (artist-draw-rect tm-target-position-x
		      tm-target-position-y
		      (+ tm-target-position-x (tm-get-target-block-length target-content))
		      (+ tm-target-position-y tm-target-width))  
    (artist-text-insert-common (+ 1 tm-target-position-x)
			       (+ (/ tm-target-width 2) tm-target-position-y) 
			       (tm-get-target-text-to-load target-content)
			       nil)))

(defun tm-load-tape (puzzle)
  "Load PUZZLE at the tape positions specified in the tm-varaibles."
  (let ((inhibit-read-only t)
	(tape-content-list nil)
	(tape-value nil)
	(cursor-pos nil)
	(current-length nil)
	(current-value nil)
	(vector-index 0)
	(x-position tm-tape-position-x))

    ;; Setting cursor in tape
    (setq tm-tape-cursor-position (tm-puzzle-initial-tape-pos puzzle))

    (setq tape-value (tm-get-initial-state-value (tm-puzzle-initial-value puzzle) (tm-puzzle-states puzzle)))
    (if (< tm-tape-cursor-position 0)
	(setq tm-tape-cursor-position (/ (length tape-value) 2))
      (setq tm-tape-cursor-position (+ tm-tape-cursor-position tm-tape-empty-states)))
    (setq cursor-pos tm-tape-cursor-position)
    (setq tape-content-list (mapcar 'string tape-value)) ;; making list from tape-contet

    (artist-text-insert-common tm-tape-position-x
			       (- tm-tape-position-y 1)
			       "Turing machine tape: " nil)

    (setq tm-tape-vector (make-vector (length tape-content-list) nil))

    (while tape-content-list
      (setq current-value (car tape-content-list))
      (setq current-length (+ 3 (length current-value)))
      (artist-draw-rect x-position
			tm-tape-position-y
			(+ x-position current-length)
			(+ tm-tape-position-y tm-tape-width))
      (artist-text-insert-common (+ x-position (/ current-length 2))
				 (+ (/ tm-tape-width 2) tm-tape-position-y)
				 current-value
				 nil)
      ;; set cursor position of tape
      (setq cursor-pos (- cursor-pos 1))
      (if (= cursor-pos 0)
	  (progn
	    (artist-text-insert-common (+ x-position (/ current-length 2))
				       (+ 1 (/ tm-tape-width 2) tm-tape-position-y)
				       "_" nil)
	    ;; set tape cursor
	    (setq tm-tape-vector-index vector-index)))

      ;; Store tape content with its position to make the navigation easier
      (aset tm-tape-vector vector-index (list (car tape-content-list) x-position tm-tape-position-y))
      (setq vector-index (+ 1 vector-index))

      ;; move to next x position
      (setq x-position (+ x-position current-length))
      ;; get next element to process
      (setq tape-content-list (cdr tape-content-list)))

    (setq tm-help-position-x (+ 2 x-position))
))

(defun tm-variable-state-p (state)
  "Return whether the given STATE has toggable value."
  (cond
   ((and (tm-state-p state) (not (tm-state-readonly state))))
   ((and (tm-direction-p state) (not (tm-direction-readonly state))))
   ((and (tm-conditional-state-p state) (not (tm-conditional-state-readonly state))))
   ((and (tm-loop-p state) (not (tm-loop-readonly state))))
   ))

(defun tm-set-variable-state-position (x y index-list)
  "Store toggleable states X Y and INDEX-LIST to tm-variable-state-position.
structure: [list (x1 y1 index-list) list(x2 y2 index-list)]."
  (let ((v-length (length tm-variable-state-positions))
	(index 0))
    (while (and (< index v-length) (elt tm-variable-state-positions index))
      (setq index (+ 1 index)))
    (if (< index v-length)
	(aset tm-variable-state-positions index (list x y index-list)))))

(defun tm-load-value (value x y)
  "Load given VALUE at position X Y."
  (let ((inhibit-read-only t))
    (artist-mode)
    (artist-text-insert-common x y value nil)
    (artist-move-to-xy x y)
    (tm-artist-mode-off)))

(defun tm-load-loop (x y state index-list)
  "Load the loop at X Y from the STATE based of INDEX-LIST."
  (let ((loop-end (tm-loop-end state))
	(set (car index-list))
	(line-start nil)
	(line-end nil)
	(loop-dist nil)
	(loop-start (cadr index-list)))
    (if (< loop-end 0)
	(setq loop-dist (* loop-end -1))
      (setq loop-dist loop-end))
    
    (if (= set 1)
	(progn
	(setq line-start (list (+ x (/ tm-state-length 2))
			       (+ 2 y tm-state-width)))

	(artist-text-insert-common (car line-start)
				   (- (cadr line-start) 1)
				   "|" nil)
	(artist-text-insert-common (car line-start)
				   (cadr line-start)
				   "|" nil)
	(if (< loop-end 0)
	    (progn
	      (setq line-end (list (- x (+ (* tm-state-length (- loop-dist 1))
					   (* loop-dist (length tm-state-interval-horizontal))
					   (/ tm-state-length 2)
					   loop-dist))
				   (+ 2 y tm-state-width)))
	      (artist-text-insert-common (car line-end)
					 (cadr line-end)
					 "|" nil)
	      
	      (artist-draw-sline (car line-start)
				 (cadr line-start)
				 (car line-end)
				 (cadr line-end))
	      (artist-text-insert-common (car line-end)
					 (- (cadr line-end) 1)
					 "^" nil)
	      )
	  (progn
	      (setq line-end (list (+ x (+ (* tm-state-length loop-dist)
					   (* loop-dist (length tm-state-interval-horizontal))
					   (/ tm-state-length 2)
					   loop-dist))
				   (+ 2 y tm-state-width)))

	      (artist-text-insert-common (car line-end)
					 (cadr line-end)
					 "|" nil)
	      
	      (artist-draw-sline (car line-start)
				 (cadr line-start)
				 (car line-end)
				 (cadr line-end))
	      (artist-text-insert-common (car line-end)
					 (- (cadr line-end) 1)
					 "^" nil)	    
	      
	      )))
      (progn
	(setq line-start (list (+ x (/ tm-state-length 2))
			       (- y 2)))

	(artist-text-insert-common (car line-start)
				   (cadr line-start)
				   "|" nil)
	(artist-text-insert-common (car line-start)
				   (+ 1 (cadr line-start))
				   "|" nil)
	(if (< loop-end 0)
	    (progn
	      (setq line-end (list (- x (+ (* tm-state-length (- loop-dist 1))
					   (* loop-dist (length tm-state-interval-horizontal))
					   (/ tm-state-length 2)
					   loop-dist))
				   (- y 2)))
	      (artist-text-insert-common (car line-end)
					 (cadr line-end)
					 "|" nil)	      
	      
	      (artist-draw-sline (car line-start)
				 (cadr line-start)
				 (car line-end)
				 (cadr line-end))
	      (artist-text-insert-common (car line-end)
					 (+ 1 (cadr line-end))
					 "v" nil))
	  (progn
	      (setq line-end (list (+ x (+ (* tm-state-length loop-dist)
					   (* loop-dist (length tm-state-interval-horizontal))
					   (/ tm-state-length 2)
					   loop-dist))
				   (- y 2)))
	      (artist-text-insert-common (car line-end)
					 (cadr line-end)
					 "|" nil)	      
	      (artist-draw-sline (car line-start)
				 (cadr line-start)
				 (car line-end)
				 (cadr line-end))
	      (artist-text-insert-common (car line-end)
					 (+ 1 (cadr line-end))
					 "v" nil)
	    ))))))

(defun tm-load-a-state (state x y index-list)
  "Load STATE at position X Y using INDEX-LIST."
  (let ((print-text nil)
	(text-pos-x nil)
	(text-pos-y nil)
	(variable-state-pos -1))

    ;; Get print string for its corresponding state
    (setq print-text (tm-get-state-print-value state))

    (unless print-text
      (setq print-text " "))

    ;; draw a block with state length and width
    (artist-draw-rect x y
		      (+ x tm-state-length)
		      (+ y tm-state-width))

    ;; text position will be middle of first line of each block
    (setq text-pos-x (+ x (+ 1 (/ (- tm-state-length (length print-text)) 2))))
    (setq text-pos-y (+ 1 y))

    ;; insert state value
    (artist-text-insert-common text-pos-x text-pos-y print-text nil)

    (if (tm-loop-p state)
	(tm-load-loop x y state index-list))

    ;; Add variable state position if the state value can be changed
    (if (tm-variable-state-p state)
	(tm-set-variable-state-position text-pos-x text-pos-y index-list))))

(defun tm-load-states (states)
  "Load set of STATES at specified in tm positions."
  (let ((first-line (car states))        ;; First line of the states
	(second-line (cadr states))      ;; Second line of the states TODO: Make it generic
	(h-interval-pos-x (+ 1 tm-state-position-x tm-state-length))   ;; h interval x: should start next to each block
	(h-interval-pos-y (+ tm-state-position-y (/ tm-state-width 2))) ;; h interval y: should be at middle of each blocks width 
	(v-interval-pos-x (+ tm-state-position-x (/ tm-state-length 2))) ;; v interval x: should be at middle of each blocks length 
	(v-interval-pos-y (+ 1 tm-state-position-y tm-state-width)) ;; v interval y: should start end of each blocks width
	(state-position-x tm-state-position-x)
	(state-position-y tm-state-position-y)
	(index 0)
	(set-position -1))

    ;; Storing position of each block to make the navigation easier
    (unless tm-state-values
      (progn
	(setq tm-state-values (list 
			       (make-vector (length first-line) nil)
			       (make-vector (length second-line) nil)))
	(setq set-position 0)))
    
    ;; Insert initial state indicator
    (artist-text-insert-common (- tm-state-position-x (length tm-initial-state-indicator))
			       (+ tm-state-position-y (/ tm-state-width 2))
			       tm-initial-state-indicator
			       nil)
    ;; Loop through first set of states
    (while first-line
      (tm-load-a-state (car first-line)
		       state-position-x
		       state-position-y
		       (list 0 index))
      (setq index (+ 1 index))
      (if (>= set-position 0)
	  (progn
	    (aset (car tm-state-values) set-position (list (car first-line) state-position-x state-position-y))
	    (setq set-position (+ 1 set-position))))

      ;; next state x position should be after block length and the horizontal interval
      (setq state-position-x (+ 1 state-position-x
    				tm-state-length
    				(length tm-state-interval-horizontal)))

      ;; Vertical interval
      (if (tm-conditional-state-p (car first-line))
	  (artist-text-insert-common v-interval-pos-x
				     v-interval-pos-y
				     tm-state-interval-vertical
				     nil))
      ;; next vertical interval should start middle of next block length 
      (setq v-interval-pos-x (+ state-position-x
				(/ tm-state-length 2)))

      ;; next state
      (setq first-line (cdr first-line))
      
      ;; Horizontal interval
      ;; If not last element, print horizontal interval
      (if first-line
    	  (progn
    	    (artist-text-insert-common h-interval-pos-x
    				       h-interval-pos-y
    				       tm-state-interval-horizontal
    				       nil)
	    ;; next horizontal interval should start after a block length
    	    (setq h-interval-pos-x (+ 1 state-position-x
    				      tm-state-length)))))

    ;; Setting help content x-position
    (if tm-help-position-x
	(if (> state-position-x tm-help-position-x)
	    (setq tm-help-position-x state-position-x))
      (setq tm-help-position-x state-position-x))

    ;;; Process second set of states
    ;; Second line initial positions
    (setq state-position-x tm-state-position-x)
    ;; next line will start after a block width + length of vertical interval
    (setq state-position-y (+ 1 tm-state-position-y
    			      tm-state-width
    			      (length tm-state-interval-vertical)))

    ;; h interval x: a block length after initial position
    (setq h-interval-pos-x (+ 1 tm-state-position-x tm-state-length))
    ;; h interval y: middle of next block width
    (setq h-interval-pos-y (+ state-position-y (/ tm-state-width 2)))

    (setq v-interval-pos-x (+ tm-state-position-x (/ tm-state-length 2)))

    (setq tm-state-count index)
    (setq index 0)

    (if (>= set-position 0)
	(setq set-position 0))

    (while second-line
      (tm-load-a-state (car second-line)
		       state-position-x
		       state-position-y
		       (list 1 index))
      (setq index (+ 1 index))
      (if (>= set-position 0)
	  (progn
	    (aset (cadr tm-state-values) set-position (list (car second-line) state-position-x state-position-y))
	    (setq set-position (+ 1 set-position))))

      (setq tm-message-max-length (+ 1 state-position-x
				     tm-state-length))
      (setq state-position-x (+ 1 state-position-x
    				tm-state-length
    				(length tm-state-interval-horizontal)))
      ;; Vertical interval
      (if (tm-conditional-state-p (car second-line))
	  (artist-text-insert-common v-interval-pos-x
				     v-interval-pos-y
				     tm-state-interval-vertical
				     nil))
      ;; next vertical interval should start middle of next block length 
      (setq v-interval-pos-x (+ state-position-x
				(/ tm-state-length 2)))

      (setq second-line (cdr second-line))
      (if second-line
	  (progn
	    (artist-text-insert-common h-interval-pos-x
				       h-interval-pos-y
				       tm-state-interval-horizontal
				       nil)
	    (setq h-interval-pos-x (+ 1 state-position-x
				      tm-state-length)))))

    ;; Setting help block width =>  end of states - starting of Target block
    (setq tm-help-block-width (+ state-position-y
				 tm-state-width))

    ;; Setting message max length
    (setq tm-message-y-pos (+ state-position-y
			      tm-state-width
			      4))
))

;; Adding empty states to initial states
(defun tm-get-initial-state-value (initial-value states)
  "Return modified INITIAL-VALUE based on the STATES."
  (let ((s-length nil)
	(s-null nil))
    (if states
	(progn
	  (setq s-length (length (car states)))
	  (unless tm-tape-empty-states
	    (setq tm-tape-empty-states (/ s-length 2)))
	  (setq s-null (make-string tm-tape-empty-states 32)))) ;; space
    (concat s-null initial-value s-null)))

;; Load turing machine puzzle
(defun tm-load-puzzle (puzzle)
  "Load turing machine PUZZLE."
  (let ((inhibit-read-only t)
	(variable-positions-xy nil)
	(tape-value nil)
	(tm-state-cursor-xy nil))
    (artist-mode)
    (artist-clear-buffer (get-buffer tm-buffer-name))
    (tm-print-header puzzle)
    (if (tm-puzzle-p puzzle)
	(progn
	  ;; Load target string
	  (tm-load-target (tm-puzzle-target puzzle))

	  ;; Setting cursor in tape
	  ;; (setq tm-tape-cursor-position (tm-puzzle-initial-tape-pos puzzle))
	  ;; (setq tape-value (tm-get-initial-state-value (tm-puzzle-initial-value puzzle) (tm-puzzle-states puzzle)))
	  ;; (if (< tm-tape-cursor-position 0)
	  ;;     (setq tm-tape-cursor-position (/ (length tape-value) 2)))
	  ;; Loading tape
	  (tm-load-tape puzzle)

	  (tm-load-states (tm-puzzle-states puzzle))

	  (setq tm-state-cursor 0)
	  ;; Setting cursor position at first variable state
	  ;; (setq tm-state-cursor-xy (elt tm-variable-state-positions 0))
	  ;; (if tm-state-cursor-xy
	  ;;     (artist-move-to-xy (car tm-state-cursor-xy) (cadr tm-state-cursor-xy)))
	  ))
    (tm-artist-mode-off)))

(defun tm-load-message (message)
  "Load given MESSAGE in turing machine game."
  (let ((inhibit-read-only t)
	(c-length (length tm-message)))
    (artist-mode)
    (if tm-message
	(artist-erase-rect nil 
			   (- tm-message-max-length c-length)
			   tm-message-y-pos
			   tm-message-max-length
			   (+ 3 tm-message-y-pos)
	))
    (setq tm-message message)
    (if tm-message
	(artist-text-insert-common (- tm-message-max-length (length tm-message))
				   tm-message-y-pos
				   tm-message
				   nil))
    (tm-artist-mode-off)
))

(defun tm-load-help ()
  "Load help block in turing machine game."
  (let ((inhibit-read-only t))
    (setq tm-help-position-y (- tm-target-position-y 1))
    (setq tm-help-block-length (- (window-body-width)
				  10))
    (if (and tm-help-content
	     tm-help-position-x
	     tm-help-position-y)
	(progn
	  (artist-mode)
	  ;; (artist-draw-rect tm-help-position-x
	  ;; 		    tm-help-position-y
	  ;; 		    tm-help-block-length
	  ;; 		    tm-help-block-width)
	  (artist-text-insert-common (+ 1 tm-help-position-x)
				     (+ 1 tm-help-position-y)
				     tm-help-content
				     nil)
	  (tm-artist-mode-off)
	  ))))

(defun tm-clean-variable-states-positions ()
  "Cleaning variable state positions list."
  (let ((index -1)
	(v-length (- (length tm-variable-state-positions) 1))
	(tm-temp-variable-vector nil))
    (if v-length
	(progn
	  (while (and (< index v-length) (elt tm-variable-state-positions (+ 1 index)))
	    (setq index (+ 1 index)))
	  (if (> index 0)
	      (setq tm-temp-variable-vector (make-vector (+ 1 index) nil)))
	  (while (>= index 0)
	    (aset tm-temp-variable-vector index (elt tm-variable-state-positions index))
	    (setq index (- index 1)))))
    (setq tm-variable-state-positions tm-temp-variable-vector)))

(defun tm-position-cursor ()
  "Positioning the cursor point in first variable state."
  (let ((tm-state-cursor-xy (elt tm-variable-state-positions 0)))
    (if tm-state-cursor-xy
	(progn
	  (artist-mode)
	  (artist-move-to-xy (car tm-state-cursor-xy) (cadr tm-state-cursor-xy))
	  (tm-artist-mode-off)))))

(defun tm-artist-mode-off ()
  "Turn off artist mode."
  (artist-mode-off)
  (artist-mode-exit))

(defun tm-get-complexity-str (puzzle)
  "Return human readable complexity string for PUZZLE."
  (if (tm-puzzle-p puzzle)
      (let ((complexity (tm-puzzle-complexity puzzle)))
	(cond ((= complexity 1) "Easy mode")
	      ((= complexity 2) "Moderate mode")
	      ((>= complexity 3) "Difficult mode")))
    ))

(defun tm-print-header (puzzle)
  "Print the header based on the given PUZZLE."
  (let ((header "Turing Machine Game : ")
	(complexity-str (tm-get-complexity-str puzzle)))
    (if complexity-str
	(setq header (concat header " (" complexity-str ") ")))
    (artist-text-insert-common tm-tape-position-x
			       2
			       header
			       nil)
    ))

(defun tm-init-puzzle (puzzle)
  "Initialize the given PUZZLE in `tm-board'."
  (if (tm-puzzle-p puzzle)
      (progn
	(setq tm-current-puzzle-source (tm-get-puzzle-copy puzzle)
	      tm-current-puzzle (tm-get-puzzle-copy puzzle)
	      tm-variable-state-positions nil
	      tm-state-values nil
	      tm-tape-vector nil
	      tm-state-count nil)

	(setq tm-variable-state-positions (make-vector (* 2 (length (car (tm-puzzle-states tm-current-puzzle)))) nil))

	(tm-load-puzzle tm-current-puzzle)

	(tm-clean-variable-states-positions)
	(tm-load-help)
	(tm-position-cursor)
	t) nil))

(defun tm-init ()
  "Initialize turing machine game."
  (setq tm-puzzle-list (tm-get-puzzle-list tm-difficulty))
  (if (and tm-current-puzzle-index (< tm-current-puzzle-index (length tm-puzzle-list)))
      (tm-init-puzzle (nth tm-current-puzzle-index tm-puzzle-list))
    (tm-init-puzzle (car tm-puzzle-list))))

(provide 'tm-board)
;;; tm-board.el ends here
