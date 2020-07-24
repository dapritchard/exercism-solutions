;;; -*- lexical-binding: t -*-
;;; grains.el --- Grains exercise (exercism)

;;; Commentary:

;;; Code:

(defun square (num)
  "Find the number of grains of rice for a given square.
Take a number between 1 and 64, inclusive, and return the number
of grains of rice on the NUM-th square of the chessboard
according to the fable where the squire asks for one grain of
rice on the first chess square, and for each remaining chess
square asks for double the number of grains on the square as for
the previous square."
  (unless
      (and
       (numberp num)
       (<= 1 num)
       (<= num 64))
    (error "NUM must be a number between 1 and 64, inclusive."))
  (expt 2.0 (1- (truncate num))))


(defun total ()
  "Find the sum total grains of rice for all the board squares.
Return the total number of grains of rice on the chessboard
according to the fable where the squire asks for one grain of
rice on the first chess square, and for each remaining chess
square asks for double the number of grains on the square as for
the previous square."
  (1- (* 2.0 (square 64.0))))


(provide 'grains)
;;; grains.el ends here
