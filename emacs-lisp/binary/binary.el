;;; -*- lexical-binding: t -*-
;;; binary.el --- Binary exercise (exercism)

;;; Commentary:

;;; Code:

(defun to-decimal (binary-string)
  "Convert a binary string to a decimal number.
Takes a string BINARY-STRING representing a number in binary form
as input and returns the decimal representation of the number as
an integer.  Elements of the string that are not 0 or 1 are
removed, and an empty string returns a value of 0."
  (unless (stringp binary-string)
    (error "The input to 'binary-string' must be a string."))
  (let*
      ((binary-string-01 (replace-regexp-in-string "[^01]" "" binary-string))
       (binary-string-01-split (split-string binary-string-01 "" t))
       (single-digit-list (mapcar #'string-to-number binary-string-01-split))
       (single-digit-rev-list (reverse single-digit-list)))
    (to-decimal--from-reverse-nums single-digit-rev-list 2 0)))


(defun to-decimal--from-reverse-nums (num-list base pow)
  "Convert a reverse sequence of values to a decimal value.
Takes a list of numbers NUM-LIST and numbers BASE and POW as
inputs.  Let us denote the elements of NUM-LIST as x_i and the
length of NUM-LIST as n, then this function calculates the
following value:

    sum_{i = 0}^{n} x_i * BASE^{POW + i}.

This formula will convert a number from base BASE to a decimal
value when the values in num-list are the digits from a given
number in reverse order, and POW is 0."
  (unless
      (and
       (listp num-list)
       (numberp base)
       (numberp pow))
    (error "Invalid inputs."))
  (if (null num-list)
      0
    (let* ((x (car num-list)))
      (unless (numberp x)
        (error "The elements of 'num-list' must be either '0' or '1'"))
      (+
       (* x (expt base pow))
       (to-decimal-from-nums (cdr num-list) base (1+ pow))))))


(provide 'binary)
;;; binary.el ends here
