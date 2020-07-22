;;; rna-transcription.el -- RNA Transcription (exercism)

;;; Commentary:

;;; Code:

(defun to-rna (dna-str)
  "Translate a DNA strand to its RNA complement.
Take a string DNA-STR as input, and return a string such that the
k-th letter of the output string corresponds to the RNA
complement of the k-th letter of the input string."
  (unless (stringp dna-str)
    (error "The input to DNA-STR must be a string."))
  (let*
      ((out-str (make-string (length dna-str) 0))
       (transl-alist '((?G . ?C)
                       (?C . ?G)
                       (?T . ?A)
                       (?A . ?U))))
    (dotimes (i (length dna-str) out-str)
      (let*
          ((from-char (elt dna-str i))
           (to-char (alist-get from-char transl-alist)))
        (unless to-char
          (error "Each element of DNA-STR must be one of A, C, G, or T."))
        (setf (elt out-str i) to-char)))))



(provide 'rna-transcription)
;;; rna-transcription.el ends here
