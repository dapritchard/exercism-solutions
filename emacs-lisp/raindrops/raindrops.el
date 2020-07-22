;;; -*- lexical-binding: t -*-
;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

(defun convert (n)
  "Convert an integer N to its raindrops string.
If N is evenly divisible by one or more of the numbers 3, 5, or
7, then it returns the string formed by concatenating
\"Pling\" (if N is divisible by 3), \"Plang\" (if N is divisible
by 5), and \"Plong\" (if N is divisible by 7) in that order.
Otherwise, it returns a string representation of N."
  (unless (integerp n)
    (error "The input to N must be an integer."))
  (let*
      ((raindrop-strings
        (list
         (if (zerop (% n 3)) "Pling")
         (if (zerop (% n 5)) "Plang")
         (if (zerop (% n 7)) "Plong")))
       (raindrop-strings-filter (seq-filter #'identity raindrop-strings)))
    (if raindrop-strings-filter
        (apply #'concat raindrop-strings)
      (number-to-string n))))


(provide 'raindrops)
;;; raindrops.el ends here
