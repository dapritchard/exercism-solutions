;;; pangram.el --- Pangram (exercism)

;;; Commentary:

;;; Code:

(defun is-pangram (s)
  "Check if string is a pangram.
Takes a string S as input, and returns t if S is a pangram, which
is defined as containing all 26 letters of the English
alphabet (case insensitive)."
  (unless (stringp s)
    (error "The input to 's' must be a string."))
  (let*
      ((letters (replace-regexp-in-string "[^[:alpha:]]" "" s))
       (letters-lower (downcase letters))
       (letters-lower-split (split-string letters-lower "" t))
       (letters-lower-split-unique (delete-dups letters-lower-split))
       (length-letters-unique (length letters-lower-split-unique)))
    (= length-letters-unique 26)))


(provide 'pangram)
;;; pangram.el ends here
