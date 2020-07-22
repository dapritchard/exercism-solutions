;;; phone-number.el --- phone-number Exercise (exercism)

;;; Commentary:

;;; Code:

(defun numbers (num-str)
  "Extract the phone number digits from a string.
Take a string NUM-STR as input, and return a string with any
non-digit characters and possibly a leading country code removed.
NUM-STR must represent a valid NANP phone number or an error is
thrown.  This means that the number (after removing any non-digit
characters) must be of the form nxxnxxxxxx (optionally with a
leading 1 prepended), and where 'n' is a digit between 2 and 9,
and 'x' is any digit."
  (unless (stringp num-str)
    "The input to NUM-STR must be a sting.")
  (let*
      ((num-10digit-str (numbers--normalize-10digits num-str)))
    (when (not (string-match-p "^[2-9]" num-10digit-str))
      (error "The first digit of the area code must be a digit from 2 through 9."))
    (when (not (string-match-p "^...[2-9]" num-10digit-str))
      (error "The first digit of the local number must be a digit from 2 through 9."))
    num-10digit-str))


(defun area-code (num-str)
  "Extract the area code digits from a string.
Take a string NUM-STR as input, and return a string with the
three digits representing the area code.  NUM-STR must represent
a valid NANP phone number or an error is thrown.  This means that
the number (after removing any non-digit characters) must be of
the form nxxnxxxxxx (optionally with a leading 1 prepended), and
where 'n' is a digit between 2 and 9, and 'x' is any digit."
  (unless (stringp num-str)
    "The input to NUM-STR must be a sting.")
  (let*
      ((num-10digit-str (numbers num-str)))
    (replace-regexp-in-string "^\\(...\\).*" "\\1" num-10digit-str)))


(defun pprint (num-str)
  "Extract and prettify the phone number digits from a string.
Take a string NUM-STR as input, and return a prettified version
of the string with any non-digit characters and possibly a
leading country code removed and some additional formatting
added.  The NUM-STR must represent a valid NANP phone number or
an error is thrown.  This means that the number (after removing
any non-digit characters) must be of the form
nxxnxxxxxx (optionally with a leading 1 prepended), and where 'n'
is a digit between 2 and 9, and 'x' is any digit.  The prettified
version is of the form (nxx) nxx-xxxx."
  (unless (stringp num-str)
    "The input to NUM-STR must be a sting.")
  (let*
      ((num-10digit-str (numbers num-str)))
    (replace-regexp-in-string "^\\(...\\)\\(...\\)\\(....\\)$" "(\\1) \\2-\\3" num-10digit-str)))


(defun numbers--normalize-10digits (num-str)
  "Remove any non-digits and a possible 11th digit from a string.
Take a string NUM-STR as input, and remove any non-digit
characters from the string, as well as a possible leading '1'
character in the event that there are exactly 11 digits
remaining.  The return string is guaranteed to be exactly 10
characters in length.  If, after removing any non-digit
characters, there are not either 10 characters remaining or 11
characters with a leading '1', then an error is thrown."
  (unless (stringp num-str)
    "The input to NUM-STR must be a sting.")
  (let*
      ((num-digits-str (replace-regexp-in-string "[^[:digit:]]" "" num-str))
       (num-digits-str-len (length num-digits-str)))
    (when (not (or (= num-digits-str-len 11) (= num-digits-str-len 10)))
        (error "NUM-STR must have either 10 or 11 digits."))
    (if (= num-digits-str-len 11)
        (progn
          (if (not (string-match-p "^1" num-str))
              (error "If NUM-STR has 11 digits, then it must lead with a 1."))
          (substring num-digits-str 1))
      num-digits-str)))


(provide 'phone-number)
;;; phone-number.el ends here
