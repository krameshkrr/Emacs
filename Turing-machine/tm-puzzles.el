;;; tm-puzzles.el --- Defining turing machine puzzle games
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
;; Defining turing machine states structure and the puzzle.
;; There are some default puzzles defined and user can also define
;; their own puzzle as below:
;;
;; (tm-add-user-puzzle <puzzle>)
;; <puzzle> should be tm-puzzle structure and it should have 
;;         initial-value    => string of initial tape content
;;         states           => (list (list <states>) (list <states>))
;;         complexity       => integer number 1..3
;;         initial-tape-pos => Initial cursor position of tape.
;;                              (-1 to set middle of tape) 
;; States can be empty, state with a value, direction state,
;; conditional state or loop.
;; Each states have its own structure as below.

;;; Code:
(eval-when-compile
   (require 'cl))

(defvar tm-puzzles nil
  "List of puzzles.")

(defvar tm-user-puzzles nil
  "List of user defined puzzles.")

(defstruct tm-state
 "State structure.
value: value of the state ex: 0, 1 or null(-1).
readonly: flag to indicate its read-only.
allowed: list of values can be allowed."
  (value 0)
  (readonly t)
  (allowed '(0 1 -1) :type list)

  ;; internal use
  (print '(lambda (state)
	   (if (tm-state-p state)
	       (let ((state-value (tm-state-value state)))
		 (if (< state-value 0)
		     (setq state-value "n")
		   (setq state-value (int-to-string state-value)))
		 (if (tm-state-readonly state)
		     state-value
		   (concat state-value "*"))
		 )))))


(defstruct tm-empty
  "Structure of empty state."
  (value 0)
  (readonly t)
  (print '(lambda (state)
	    (if (tm-empty-p state)
		" "))))

(defstruct tm-direction
  "Direction indicator structure.
value     => direction value 0 => left, 1 => right
readonly  => If not readonly, value can be set from allowed
allowed   => list of values can be set if not readonly."
  (value 0)
  (readonly t)
  (allowed '(0 1) :type list)

  ;; internal use
  (print '(lambda (direction-state)
	    (if (tm-direction-p direction-state)
		(let ((dir-value (tm-direction-value direction-state))
		      (print-val nil))
		  (if (= dir-value 0)
		      (setq print-val "<-"))
		  (if (= dir-value 1)
		      (setq print-val "->"))
		  ;; (if (= dir-value -1)
		  ;;     (setq print-val "^"))
		  ;; (if (= dir-value -2)
		  ;;     (setq print-val "v"))
		  (if (tm-direction-readonly direction-state)
		      print-val
		    (concat print-val "*"))
	      )))))

(defstruct tm-conditional-state
  "Conditional state.
value => Value to check the condition.
         If tape content match with this value
         then move to this state direction.
direction => Condition true direction.
readonly  => If not readonly, it can be set from allowed
allowed   => list of values can be set if not readonly."
  (value 0)
  (direction 0)
  (readonly nil)
  (allowed '(0 1 -1) :type list)

  ;; internal use
  (print '(lambda (conditional-state)
	    (if (tm-conditional-state-p conditional-state)
		(let ((dir-value (tm-conditional-state-direction conditional-state))
		      (val (tm-conditional-state-value conditional-state))
		      (print-val nil))
		  (if (< val 0)
		      (setq val "n")
		    (setq val (int-to-string val)))
		  ;; (if (= dir-value 0)
		  ;;     (setq print-val "<-"))
		  ;; (if (= dir-value 1)
		  ;;     (setq print-val "->"))
		  (if (= dir-value -1)
		      (setq print-val "^"))
		  (if (= dir-value -2)
		      (setq print-val "v"))
		  (if (tm-conditional-state-readonly conditional-state)
		      (concat val print-val)
		    (concat val "*" print-val))
	       )))))

(defstruct tm-loop
  "Structure of a tm-loop.
value   => Loop condition value.
end     => The distance of loop end state.
           (negative => left, positive => right)
readonly => If not readonly, the value can be set from allowed."
  (value 0)
  (end 0)
  (readonly t)
  (allowed '(0 1 -1) :type list)
  (print '(lambda (loop-state)
	    (let ((val (tm-loop-value loop-state)))
	      (cond ((= val -2) (setq val "oo"))
		    ((< val 0) (setq val "n"))
		    (t (setq val (int-to-string val))))
	      (if (tm-loop-readonly loop-state)
		  (concat  val "?")
		(concat val "*?"))
	      ))))

(defstruct tm-puzzle
  "Structure of a tm-puzzle.
states => list of states in the puzzle.
complexity => the difficulty level of the puzzle.
target => The target string to be achieved.
initial-value => The initial tape content.
initial-tape-pos => The initial tape cursor position
                    (-1 indicates middle of the tape)."
  (states '() :type list)
  (complexity 1)
  (target "0")
  (initial-value "0")
  (initial-tape-pos -1)) ;; -1 for middle of initial-value

;; Get functions
(defun tm-get-state-value (state)
  "Get value of the STATE."
  (cond
   ((tm-empty-p state) (tm-empty-value state))
   ((tm-state-p state) (tm-state-value state))
   ((tm-direction-p state) (tm-direction-value state))
   ((tm-conditional-state-p state) (tm-conditional-state-value state))
   ((tm-loop-p state) (tm-loop-value state))
   ))

(defun tm-get-state-print-value (state)
  "Return print string of the given STATE."
  (cond
   ((tm-empty-p state) (funcall (tm-empty-print state) state))
   ((tm-state-p state) (funcall (tm-state-print state) state))
   ((tm-direction-p state) (funcall (tm-direction-print state) state))
   ((tm-conditional-state-p state) (funcall (tm-conditional-state-print state) state))
   ((tm-loop-p state) (funcall (tm-loop-print state) state))
   ))

(defun tm-get-state-allowed-list (state)
  "Return allosed list of the given STATE."
  (cond
   ((tm-state-p state) (tm-state-allowed state))
   ((tm-direction-p state) (tm-direction-allowed state))
   ((tm-conditional-state-p state) (tm-conditional-state-allowed state))
   ((tm-loop-p state) (tm-loop-allowed state))
   ))

;; Set functions
(defun tm-set-state-value (value state)
  "Set VALUE for given STATE."
  (cond
   ((tm-state-p state) (setf (tm-state-value state) value))
   ((tm-direction-p state) (setf (tm-direction-value state) value))
   ((tm-conditional-state-p state) (setf (tm-conditional-state-value state) value))
   ((tm-loop-p state) (setf (tm-loop-value state) value))
   ))

(defun tm-validate-puzzle (puzzle)
  "Validating the puzzle."
  t
)

(defun tm-add-puzzle (puzzle)
  "Add given PUZZLE to default puzzle lsit."
  (if (and (tm-puzzle-p puzzle) (tm-validate-puzzle puzzle))
      (setq tm-puzzles (cons puzzle tm-puzzles))))

(defun tm-add-user-puzzle (puzzle)
  "Add given PUZZLE to user defined puzzles."
  (if (and (tm-puzzle-p puzzle) (tm-validate-puzzle puzzle))
      (setq tm-user-puzzles (cons puzzle tm-user-puzzles))))

(defun tm-set-default-puzzles ()
  "Set default puzzles to puzzles-list."
  (let* (
	 (empty (make-tm-empty))

	 (s1v (make-tm-state :value 1 :readonly nil :allowed '(0 1)))
	 (s0v (make-tm-state :value 0 :readonly nil :allowed '(0 1)))
	 (s1r (make-tm-state :value 1 :readonly t))
	 (s0r (make-tm-state :value 0 :readonly nil :allowed '(0 1 -1)))

	 (dR (make-tm-direction :value 1))
	 (dL (make-tm-direction :value 0))

	 (c0vd (make-tm-conditional-state :value 0 :readonly nil :direction -2 :allowed '(0 1 -1)))
	 (cnvU (make-tm-conditional-state :value -1 :direction -1 :readonly nil :allowed '(0 1 -1)))
	 (cnrD (make-tm-conditional-state :value -1 :direction -2 :readonly t))
	 (c0vU (make-tm-conditional-state :value 0 :readonly nil :allowed '(0 1 -1) :direction -1))

	 (loop-4 (make-tm-loop :value -2 :readonly t :end -4))
	 (loop-2 (make-tm-loop :value -2 :readonly t :end -2))
	 (loop2 (make-tm-loop :value -2 :readonly t :end 2))

	 (puzzle nil)
	 (puzzle-states nil))
    
    (setq puzzle-states (list (list dL s1v dR dR dR s1v)
			  (list empty empty empty empty empty empty)))
    (setq puzzle (make-tm-puzzle :states puzzle-states :initial-value "00010" :target "01011" :initial-tape-pos 3 :complexity 1))
    (tm-add-puzzle puzzle)

    (setq puzzle-states (list (list dL dL c0vd s0v dR cnrD loop-4)
			  (list empty empty s0v empty c0vU empty empty)))
    (setq puzzle (make-tm-puzzle :states puzzle-states :initial-value "01111" :target "10000" :initial-tape-pos 3 :complexity 2))
    (tm-add-puzzle puzzle)
    
    (setq puzzle-states (list (list dL c0vd loop-2 empty dL s1r dL s0r)
			  (list empty empty s0r dR cnvU loop-2)))
    (setq puzzle (make-tm-puzzle :states puzzle-states :initial-value "01 01" :target "00011" :initial-tape-pos 3 :complexity 3))
    (tm-add-puzzle puzzle)
))

(defun tm-get-puzzles ()
  "Returning all puzzles."
  (setq tm-puzzles nil)
  (tm-set-default-puzzles)
  (if (and tm-puzzles tm-user-puzzles)
      (append (reverse tm-puzzles) (reverse tm-user-puzzles))
    (if tm-puzzles
	(reverse tm-puzzles)
      (if tm-user-puzzles
	  (reverse tm-user-puzzles)))))

(defun tm-get-puzzle-list (difficulty)
  "Return list of puzzles for given DIFFICULTY."
  (let ((puzzles-list (tm-get-puzzles))
	(difficulty-list nil)
	(iterator nil))
    (if (and difficulty (> difficulty 0) (< difficulty 4))
	(progn
	  (setq iterator puzzles-list)
	  (while iterator
	    (if (and (tm-puzzle-p (car iterator))
		     (= (tm-puzzle-complexity (car iterator)) difficulty))
		  (setq difficulty-list (cons (car iterator) difficulty-list)))
	    (setq iterator (cdr iterator)))))

    (if difficulty-list
	difficulty-list puzzles-list)))

(provide 'tm-puzzles)
;;; tm-puzzles.el ends here
