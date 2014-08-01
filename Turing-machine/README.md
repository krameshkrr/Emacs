<h1>Turing Machine Puzzle Game</h1>

Turing machine is a computing machine that can use a predefined set of rules to determine a result from a set of input variables.

<h2>Install</h2>
    Copy all turing machine packages to your load path.
    
    Add below line to your emacs init file.
    
    (require 'turing-machine)

<h2>Dependencies</h2>
    artist-mode

<h2>How to play</h2>
To start the game M-x turing-machine and a puzzle will be loaded. Navigate through the states using left/right arrow and change its values using space and hit 'p' to process the states and match it with target. Help is always shown in game buffer as below.
    
Screen Shot:
![alt tag](https://raw.githubusercontent.com/krameshkrr/Emacs/master/Turing-machine/Turing-machine.png)

<h2>Add puzzles</h2>
Define your own puzzle in the structure defined in tm-puzzles.el and add it to user defined puzzles as below.
```
(defvar tm-my-puzzle nil)

(let* ((empty-state (make-tm-empty)            ;; empty state
       (state (make-tm-state :value 0          ;; state with value 0 and it can vary between null, 0 and 1
                             :readonly nil 
                             :allowed '(-1 0 1)))
       (conditional (make-conditional-state :value 1     ;; condition state with true value 1 and it can vary between 0 and 1
                                            :readonly nil 
                                            :allowed '(0 1)))
       (direction (make-tm-direction :value 0))      ;; left direction state
       (loop (make-tm-loop :value 0 :end -2 :readonly t))   ;; loop with true value 0 and it end state is 2 states after in left direction
       (puzzle-states (list (list state direction conditional loop)
                            (list empty state state empty)))
       ))
    (setq tm-my-puzzle (make-tm-puzzle :states puzzle-states      ;; puzzle states 
                                       :initial-value "001010"    ;; initial value of the turing machine tape
                                       :target "110011"           ;; Target which should be achievec
                                       :initial-tape-pos 3        ;; Initial tape cursor position
                                       :complexity 2))            ;; Difficulty level. It should be 1 to 3
)

(tm-add-user-puzzles tm-my-puzzle)
```
<h3>Contact</h3>
Email: krameshkrr@gmail.com
