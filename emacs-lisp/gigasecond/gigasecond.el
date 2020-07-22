;;; gigasecond.el --- Gigasecond exercise (exercism)

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:

(defun from (second minute hour day month year)
  "Calculate the time 1 gigasecond from now.
Takes inputs of the same form as the first elements in
`encode-time', and returns a list of the same form as the first
elements in `decode-time'."
  (let* ((time (encode-time second minute hour day month year nil nil "UTC0")))
    (seq-take
     (decode-time
      (time-add time 1000000000)
      "UTC0")
     6)))


(provide 'gigasecond)
;;; gigasecond.el ends here
