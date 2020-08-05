;;; -*- lexical-binding: t -*-
;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)

;;; Commentary:

;;; Code:

(defun armstrong-p (n)
  "Check for an Armstrong number.
Take a natural number N and return `t' if it is an Armstrong
number, and `nil' otherwise."
  (unless
      (and
       (integerp n)
       (> n 0))
    (error "N must be an integer no less than 1."))
  (= n (armstrong-number n)))


(defun armstrong-number (n)
  "Find the value upon which an Armstrong number is determined.
Take a natural number N and return the value upon which an
Armstrong number is determined, which is defined as the sum of
all of the number's digits after taking them to the power of the
total number of digits."
  (unless
      (and
       (integerp n)
       (> n 0))
    (error "N must be an integer no less than 1."))
  (let*
      ((reverse-digits-list (armstrong-number--create-reverse-digits-list n))
       (reverse-digits-list-len (length reverse-digits-list))
       (expt-partial-fcn (lambda (x y) (+ x (expt y reverse-digits-list-len)))))
    (seq-reduce expt-partial-fcn reverse-digits-list 0)))


(defun armstrong-number--create-reverse-digits-list (n)
  "Create a list of a number's digits in reverse order.
Take a nonnegative number N as input and return a list such that
each element of the list is one of the digits of the number, and
where collectively the digits are in reverse order of the
original number."
  (unless (wholenump n)
    (error "N must be a nonnegative integer."))
  (unless (zerop n)
    (cons
     (% n 10)
     (armstrong-number--create-reverse-digits-list (/ n 10)))))


(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
