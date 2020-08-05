;;; acronym.el --- Acronym (exercism)

;;; Commentary:

;;; Code:

(defun acronym (str)
  "Create an acronym from a string.
Take a string STR as input, and return a string providing the
acronym created from it."
  (unless (stringp str)
    (error "STR must be a string."))
  (let*
      ((word-list (split-string str "[^[:alpha:]]+" t))
       (first-letters-list (acronym--extract-first-letters word-list))
       (first-letters (concat first-letters-list))
       (first-letters-uppercase (upcase first-letters)))
    first-letters-uppercase))


(defun acronym--extract-first-letters (word-list)
  "Extract the first character from each string in a list.
Take a list of strings WORD-LIST, and return a list of the first
characters from each word in WORD-LIST."
  (unless
      (or (null word-list)
          (and (listp word-list)
               (stringp (car word-list))))
    (error "WORD-LIST must be a list of strings."))
  (when word-list
    (cons
     (elt (car word-list) 0)
     (acronym--extract-first-letters (cdr word-list)))))


(provide 'acronym)
;;; acronym.el ends here
