Soduku: a sudoku solver

To run this, you'll need a LISP interperter. The easiest way to achieve this is by using lispbox. It can be found at https://common-lisp.net/project/lispbox/.

Put the solver.lisp file and an unsolved puzzle in the same dir as lispbox. Once lispbox is started run (load "solver.lisp"). The program will output the unsolved puzzle and a solved version once the program is done working.

Puzzle files should be space-separated. Empty values are represented with 0's.
