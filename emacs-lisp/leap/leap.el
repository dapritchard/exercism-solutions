;;; leap.el --- Leap exercise (exercism)

;;; Commentary:

;;; Code:

(defun leap-year-p (year)
  "Check whether a given year is a leap year."
  (unless (integerp year)
    (error "YEAR must be an integer."))
  (cond
   ((not (zerop (% year 4))) nil)
   ((zerop (% year 400)) t)
   ((zerop (% year 100)) nil)
   (t t)))


(provide 'leap-year-p)
;;; leap.el ends here
