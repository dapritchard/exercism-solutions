;;; -*- lexical-binding: t -*-
;;; crypto-square.el --- Crypto Square (exercism)

;;; Commentary:

;;; Code:

(defun encipher (str)
  "Encrypt a string using the 'crypto square' technique."
  (unless (stringp str)
    (error "The input to STR must be a string."))
  (let*
      ((lettersonly-str (replace-regexp-in-string "[^[:alnum:]]" "" str))
       (norm-str (downcase lettersonly-str))
       (norm-str-len (length norm-str))
       (ncol (ceiling (sqrt (length norm-str))))
       (nrow (if (<= norm-str-len (* ncol (1- ncol))) (1- ncol) ncol))
       (ndeficient (- (* nrow ncol) norm-str-len))
       (padded-str (concat norm-str (make-string ndeficient ?\s)))
       (col-list (encipher--create-cols padded-str ncol))
       (combined-str (mapconcat #'identity col-list " ")))
    combined-str))


(defun encipher--create-cols (str ncol)
  "Create the columns of a crypto square.
Take a string STR and integer NCOL providing the number of
columns to partition STR into, and return a list of strings such
that each element is one of the columns in the crypto square."
  (unless
      (and (stringp str)
           (wholenump ncol))
    (error "Invalid inputs to `encipher--create-cols'."))
  (let*
      ((create-col-fcn
        (lambda (start-elt)
          (concat (encipher--create-cols-single-list str start-elt ncol))))
       (start-elt-list (number-sequence 0 (1- ncol))))
    (seq-map create-col-fcn start-elt-list)))


(defun encipher--create-cols-single-list (str start-elt nskip)
  "Create a list of characters representing a crypto square column.
Take a string STR and integers START-ELT and NSKIP providing the
index of the first element of STR to include in the column and
the amount to add to get the next index in STR for the column as
inputs, and return a list of characters that collectively
comprise the tail portion of one of the columns in the crypto
square."
  (unless
      (and (stringp str)
           (wholenump start-elt)
           (wholenump nskip))
    (error "Invalid inputs to `encipher--create-cols-single-list'."))
  (when (< start-elt (length str))
    (cons (aref str start-elt)
          (encipher--create-cols-single-list str (+ start-elt nskip) nskip))))


(provide 'crypto-square)
;;; crypto-square.el ends here
