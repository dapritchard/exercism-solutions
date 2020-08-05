;;; -*- lexical-binding: t -*-
;;; trinary.el --- Trinary (exercism)

;;; Commentary:

;;; Code:

(defun trinary-to-decimal (str)
  "Convert a string representing a base-3 number to base-10.
Take a string STR as input and return an integer value.  If the
input to STR contains any characters that aren't found in a
base-3 number, then 0 is returned."
  (unless (stringp str)
    (error "STR must be a string."))
  (if (string-match-p "[^012]" str)
      0
    (let*
        ((str-len (length str))
         (total 0))
      (dotimes (i str-len total)
        (let*
            ((digit (- (aref str i) ?0))
             (power (1- (- str-len i)))
             (multiplier (expt 3 power))
             (base-10-val (* digit multiplier)))
          (setq total (+ total base-10-val)))))))


(provide 'trinary)
;;; trinary.el ends here
