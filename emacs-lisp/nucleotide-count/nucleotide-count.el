;;; nucleotide-count.el --- nucleotide-count Exercise (exercism)

;;; Commentary:

;;; Code:

(defun nucleotide-count (nucl-str)
  "Count the number of times each nucleotide occurs.
Take a string NUCL-STR as input and return an association list of
the counts of each nucleotide."
  (unless (stringp nucl-str)
    (error "The input to NUCL-STR must be a string."))
  (let*
      ((A 0)
       (C 0)
       (G 0)
       (T 0))
    (dotimes (i (length  nucl-str))
      (let* ((nucl-let (aref nucl-str i)))
        (cond
         ((eq nucl-let ?A) (setq A (1+ A)))
         ((eq nucl-let ?C) (setq C (1+ C)))
         ((eq nucl-let ?G) (setq G (1+ G)))
         ((eq nucl-let ?T) (setq T (1+ T)))
         (t (error "The elements of the input to NUCL-STR must each be one of 'A', 'C', 'G', or 'T'.")))))
    `((?A . ,A) (?C . ,C) (?G . ,G) (?T . ,T))))


(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
