;;; hamming.el --- Hamming (exercism)

;;; Commentary:

;;; Code:

(defun hamming-distance (x y)
  "Calculate the Hamming distance.
Take two strings X and Y with letters representing DNA
nucleotides, and return an integer providing the Hamming
distance."
  (unless
      (and
       (stringp x)
       (stringp y)
       (= (length x) (length y)))
    (error "X and Y must each be strings, and have equal length."))
  (let*
      ((distance-val 0)
       (allowed-vals '(?A ?C ?G ?T)))
    (dotimes (i (length x) distance-val)
      (let*
          ((val-x (elt x i))
           (val-y (elt y i)))
        (unless
            (and
             (memq val-x allowed-vals)
             (memq val-y allowed-vals))
          (error "The values in X and Y must all be one of A, C, G, or T."))
        (unless (= val-x val-y)
          (setq distance-val (1+ distance-val)))))))


(provide 'hamming)
;;; hamming.el ends here
