;;; -*- lexical-binding: t -*-
;;; bob.el --- Bob exercise (exercism)

;;; Commentary:

;;; Code:

(defun response-for (str)
  "Respond to a question or statement.
Take a string STR as input and returns one of the strings listed
below depending on the form of the input (and where non-alphabet
characters are ignored, with the exception of a possible trailing
question mark).

* \"Sure.\", if the input is a question.
* \"Whoa, chill out!\", if the input is all capital letters.
* \"Calm down, I know what I'm doing!\", if the input is all
  capital letters ending with a question mark.
* \"Fine. Be that way!\", if the input is empty.
* \"Whatever.\", if the input is anything else.

Also note the need for temporarily binding `case-fold-search' to
nil base on the following:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-and-Case.html"
  (unless (stringp str)
    (error "STR must be a string."))
  (if (string= (replace-regexp-in-string "[[:space:]]" "" str) "")
      "Fine. Be that way!"
    (let*
        ((case-fold-search nil)
         (is-yelled (and
                     (string= (upcase str) str)
                     (string-match-p "[[:upper:]]" str)))
         (is-question (string-match-p "?[[:space:]]*\\'" str)))
      (cond
       ((and (not is-yelled) (not is-question)) "Whatever.")
       ((and (not is-yelled) is-question) "Sure.")
       ((and is-yelled (not is-question)) "Whoa, chill out!")
       ((and is-yelled is-question) "Calm down, I know what I'm doing!")))))


(provide 'bob)
;;; bob.el ends here
