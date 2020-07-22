;;; two-fer.el --- Two-fer Exercise (exercism)

;;; Commentary:

;;; Code:

(defun two-fer (&optional name)
  "Create a 'two-fer' string.
Takes an optional input NAME and returns a string of the form
\"One for <NAME>, one for me.\", and where NAME is attempted to
be coerced to a string."
  (let ((name-norm (or name "you")))
    (format "One for %s, one for me." name-norm)))

(provide 'two-fer)
;;; two-fer.el ends here
