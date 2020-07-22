;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;; Code:

(defun difference (n)
  "Calculate the difference of between sums of squares
Find the value of the square of the sum of the first `n' whole
numbers and the sum of the square of the first `n' whole numbers."
  (unless (or (integerp n) (>= n 0))
    (error "The input to 'n' must be an integer no less than 0."))
  (- (square-of-sum n) (sum-of-squares n)))


(defun square-of-sum (n)
  "Calculate the square of the sum of a sequence
Takes a nonnegative integer `n' as input and calculates the
square of the sum of the first `n' whole numbers."
  ;; calculate the sum of the first `n' integers
  (unless (or (integerp n) (>= n 0))
    (error "The input to 'n' must be an integer no less than 0."))
  (square (apply '+ (number-sequence 1 n))))


(defun sum-of-squares (n)
  "Calculate the sum of the squares of a sequence
Takes a nonnegative integer `n' as input and calculates the sum
of the squares of the first `n' whole numbers."
  (unless (or (integerp n) (>= n 0))
    (error "The input to 'n' must be an integer no less than 0."))
  (apply '+ (mapcar 'square (number-sequence 1 n))))


(defun square (n)
  "Calculate the square of a number"
  (* n n))


(provide 'difference-of-squares)
;;; difference-of-squares.el ends here

(defun f1 ()
  (defun f2 () 2)
  1)
