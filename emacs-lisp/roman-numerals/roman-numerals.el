;;; roman-numerals.el --- roman-numerals Exercise (exercism)

;;; Commentary:

;;; Code:

(defun to-roman (num)
  "Convert a number to its Roman numeral representation.
Take a number NUM as input and return a string of its Roman
numeral representation."
  (unless
      (and
       (integerp num)
       (<= 1 num)
       (<= num 3000))
    (error "NUM must be an integer between 1 and 3000."))
  (concat
   (elt to-roman--thousands-list (to-roman--extract-digit num 1000))
   (elt to-roman--hundreds-list (to-roman--extract-digit num 100))
   (elt to-roman--tens-list (to-roman--extract-digit num 10))
   (elt to-roman--ones-list (to-roman--extract-digit num 1))))


(defun to-roman--extract-digit (num divisor)
  "Divide by a power of 10 do extract a given digit.
Take numbers NUM and DIVISOR as inputs.  If DIVISOR is has a
value equivalent to 10^x, then this function has then effect of
extracting the x-th digit of a number."
  (unless
      (and
       (integerp num)
       (integerp divisor))
    (error "NUM and DIVISOR must both be integers."))
  (%
   (/ num divisor)
   10))


(defconst to-roman--ones-list
  '("" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX")
  "A list of the Roman numerals from 1 to 9.")


(defconst to-roman--tens-list
  '("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC")
  "A list of the Roman numerals for the tens digit.")


(defconst to-roman--hundreds-list
  '("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM")
  "A list of the Roman numerals for the hundreds digit.")


(defconst to-roman--thousands-list
  '("" "M" "MM" "MMM")
  "A list of the Roman numerals for the thousands digit.")


(provide 'roman-numerals)
;;; roman-numerals.el ends here
