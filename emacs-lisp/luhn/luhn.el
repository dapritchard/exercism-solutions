;;; luhn.el --- Luhn exercise (exercism)

;;; Commentary:

;;; Code:

(defun luhn-p (str)
  "Check whether a number satisfies the Luhn algorithm.
Take a string NUM-STR comprised of only digits, and return either
`t' or `nil' depending on whether the number represented by
NUM-STR satisfies the Luhn algorithm."
  (unless (stringp str)
    (error "The input to STR must be a string."))
  (when (string-match-p "[^[:digit:][:space:]]" str)
    (error "The input to STR must contain only digits and spaces."))
  (let*
      ((digitsonly-str (replace-regexp-in-string "[^[:digit:]]" "" str)))
    (unless (<= (length digitsonly-str) 1)
      (zerop (% (luhn--calc-sum digitsonly-str 0)
                10)))))


(defun luhn--calc-sum (num-str elt)
  "Calculate the Luhn sum.
Take a string NUM-STR comprised of only digits, and an index ELT
with respect to the STR, and return the Luhn sum of all of the
digits including and to the right of the index, and where the
index is taken from left to right."
  (unless
      (and (stringp num-str)
           (wholenump elt))
    (error "Invalid inputs to `luhn--calc-sum'."))
  (if (>= elt (length num-str))
      0
    (+ (luhn--calc-digit num-str elt)
       (luhn--calc-sum num-str (1+ elt)))))


(defun luhn--calc-digit (num-str elt)
  "Calculate a digit's value in the Luhn sum.
Take a string NUM-STR comprised of only digits, and an index ELT
with respect to the STR, and return an integer providing the
value of the digit corresponding to the index in the Luhn sum."
  (unless
      (and (stringp num-str)
           (wholenump elt))
    (error "Invalid inputs to `luhn--calc-digit'."))
  (let* ((digit (- (aref num-str elt) ?0)))
    (if (oddp (- (length num-str) elt))
        digit
      (if (< digit 4)
          (* 2 digit)
        (- (* 2 digit) 9)))))


(provide 'luhn)
;;; luhn.el ends here
