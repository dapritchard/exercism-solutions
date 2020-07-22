;;; -*- lexical-binding: t -*-
;;; anagram.el --- Anagram (exercism)

;;; Commentary:

;;; Code:

(defun anagrams-for (word word-dictionary)
  "Find the anagrams of a word from among a list.
Take a string WORD and a list of strings WORD-DICTIONARY and
return a list consisting of strings the in WORD-DICTIONARY that
are (case-insensitive) anagrams of WORD (but not case-insensitive
exact matches)."
  (unless
      (and
       (stringp word)
       (listp word-dictionary))
    (error "Invalid inputs."))
  (let*
      ((word-lowercase (downcase word))
       (word-lowercase-sorted (sort-string word-lowercase))
       (anagram-p (anagrams-for--make-anagram-p
                   word-lowercase
                   word-lowercase-sorted))
       (anagram-lowercase-p (lambda (x) (funcall anagram-p (downcase x)))))
    (seq-filter anagram-lowercase-p word-dictionary)))


(defun anagrams-for--make-anagram-p (word word-sorted)
  "Create a predicate that checks for a word anagram.
Take strings WORD and WORD-SORTED as inputs, where WORD-SORTED is
assumed to contain all of the letters in WORD but with the
characters sorted according to `<'."
  (unless
      (and
       (stringp word)
       (stringp word-sorted))
    (error "WORD and WORD-SORTED must both be strings."))
  (lambda (new-word)
    (unless (stringp new-word)
      (error "Each element in WORD-DICTIONARY must be a string."))
    (and
       (equal (sort-string new-word) word-sorted)
       (not (equal new-word word)))))


(defun sort-string (str)
  "Sort the characters in a string.
Take a string STR as input, and return a string containing the
same characters as the input string, but with the characters
sorted according to `<'."
  (unless (stringp str)
    (error "STR must be a string."))
  (let*
      ((str-split-list (split-string str "" t))
       (letters-list (mapcar #'string-to-char str-split-list))
       (letters-list-sorted (sort letters-list #'<))
       (str-sorted (concat letters-list-sorted)))
    str-sorted))


(provide 'anagram)
;;; anagram.el ends here
