;;; -*- lexical-binding: t -*-
;;; word-count.el --- word-count Exercise (exercism)

;;; Commentary:

;;; Code:

(defun word-count (str)
  "Count the number of occurrences of each word.
Take a string STR as input, and return an alist with each element
a cons cell such that the car is the word, and the cdr is the
number of times the word occurs in the string."
  (unless (stringp str)
    (error "STR must be a string."))
  (let*
      ((lowercase-str (downcase str))
       (nopunct-str (replace-regexp-in-string "[^[:blank:][:alnum:]]" "" lowercase-str))
       (words-list (split-string nopunct-str "[[:blank:]]+" t))
       (counts-alist '()))
    (dolist (w words-list counts-alist)
      (let*
          ((w-count (cdr (assoc-string w counts-alist))))
        (if w-count
            (setcdr (assoc-string w counts-alist) (1+ w-count))
          (push (cons w 1) counts-alist))))))


(provide 'word-count)
;;; word-count.el ends here
