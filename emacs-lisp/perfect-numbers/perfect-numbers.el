;; perfect-numbers.el --- perfect-numbers Exercise (exercism)

;;; Commentary:

;;; Code:

(defun classify (number)
  "Classify a number according to its aliquot sum.
Take an integer NUMBER with a value no less than 1, and return one of the following:
* deficient:  the aliquot sum is less than the number
* perfect:    the aliquot sum is equal to the number
* abundant:   the aliquot sum is greater than the number"
  (unless
      (and
       (integerp number)
       (>= number 1))
    (error "Classification is only possible for natural numbers"))
  (let* ((aliquot-sum (calc-aliquot-sum number)))
    (cond
     ((< aliquot-sum number) 'deficient)
     ((= aliquot-sum number) 'perfect)
     ((> aliquot-sum number) 'abundant))))


(defun calc-aliquot-sum (number)
  "Calculate the aliquot sum of a number.
The aliquot sum is defined as the sum of the factors of a number,
not including the number itself."
  (unless
      (and
       (integerp number)
       (>= number 1))
    (error "NUMBER must be an integer >= 1"))
  (let*
      ((potential-divisors (number-sequence 1 (/ number 2)))
       (aliquot-sum 0))
    (dolist (k potential-divisors aliquot-sum)
      (if (zerop (% number k))
          (setq aliquot-sum (+ aliquot-sum k))))))


(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
