;;; -*- lexical-binding: t -*-
;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)

;;; Commentary:

;;; Code:

(defun run-length-encode (str)
  "Compress a string using run-length encoding.
Take a string STR as input, and return a compressed version of
the string using run-length encoding."
  (unless
      (and (stringp str)
           (not (string-match-p "[^[:alpha:][:space:]]" str)))
    (error "The input to STR must only consist of characters and whitespace."))
  (if (equal str "")
      ""
    (apply #'concat
           (run-length-encode--impl
            str
            (length str)
            (seq-elt str 0)
            1
            1))))


(defun run-length-decode (str)
  "Decompress a string that uses run-length encoding.
Take a string STR as input, and return the string equal to the
one that was originally compressed using run-length encoding."
  (unless
      (and (stringp str)
           (not (string-match-p "[^[:alnum:][:space:]]" str)))
    (error "The input to STR must only consist of numbers, characters, and whitespace."))
  (let*
      ((segm-list (run-length-decode--split-string str (length str) 0)))
    (mapconcat #'run-length-decode--decode-segm segm-list "")))


(defun run-length-encode--impl (str str-len letter count elt)
  "Internal version of `run-length-encode'.
Take a string STR, nonnegative integer STR-LEN providing the
length of STR, a letter LETTER providing the letter for the
current run of letters, a natural number COUNT providing how many
times we've seen LETTER in the current run, and a nonnegative
number ELT providing the index of the next letter in STR.  Return
a string in run-length encoding for the letters in STR starting
at element ELT, plus whatever elements were part of the
immediately preceeding run, as encoded by the LETTER and COUNT
arguments."
  (unless
      (and (stringp str)
           (wholenump str-len)
           (characterp letter)
           (and (integerp count) (>= count 1))
           (wholenump elt)
           )
    (error "Invalid inputs to `run-length-encode--impl'."))
  (if (>= elt str-len)
      ;; case: we've processed the entire string, now return the final run
      (list (run-length-encode--run-to-str count letter))
    (let*
        ((new-letter (seq-elt str elt)))
      (if (= new-letter letter)
          ;; case: we're still in part of a run so CONS an empty string with the
          ;; value obtained by recursively invoking `run-length-encode--impl'
          ;; with COUNT incremented by 1
          (cons ""
                (run-length-encode--impl
                 str
                 str-len
                 letter
                 (1+ count)
                 (1+ elt)))
        ;; case: the current run ended, so cons a string representing the
        ;; previous run with the value obtained by recursively invoking
        ;; `run-length-encode--impl' with COUNT starting at 1 again
        (cons (run-length-encode--run-to-str count letter)
              (run-length-encode--impl
               str
               str-len
               new-letter
               1
               (1+ elt)))))))


(defun run-length-encode--run-to-str (count letter)
  "Convert information about a run-length encoding into a string.
Take an integer COUNT and a character LETTER, and creates a
string with COUNT LETTERS in it."
  (unless
      (and (and (integerp count) (>= count 1))
           (characterp letter))
    ("Invalid inputs to `run-length-encode--run-to-str'."))
  (if (= count 1)
      (string letter)
    (concat (number-to-string count) (string letter))))


(defun run-length-decode--split-string (str str-len start)
  "Split a run-length encoded string into segments.
Take a string STR, a nonnegative integer STR-LEN providing the
length of STR, and a nonnegative integer START providing the
index at which to start splitting STR into segments.  Return a
list of strings such that the first string is the segment
starting at START, the second string is the following segment,
and so on."
  (unless
      (and (and (stringp str)
                (not (string-match-p "[^[:alnum:][:space:]]" str)))
           (wholenump str-len)
           (wholenump start))
    (error "Invalid inputs to `run-length-decode--split-string'."))
  (unless (>= start str-len)
    (let*
        ((end-or-nil (string-match "[^[:digit:]]" str start))
         (end (if end-or-nil (1+ end-or-nil) str-len)))
      (cons (substring str start end)
            (run-length-decode--split-string str str-len end)))))


(defun run-length-decode--decode-segm (segm)
  "Decompress a run-length encoded segment.
Take a string SEGM as input that is assumed to possibly start
with a series of digits, and have a single trailing nondigit
character.  Return a string that consists entirely of the
trailing character, and where the number of characters is given
by the value of the leading digits in the string (if there are no
leading digits then this is considered an implicit 1)."
  (unless
      (and (stringp segm)
           (string-match-p "^\\([[:digit:]]*\\)?[^[:digit:]]$" segm))
    ("Invalid inputs to `run-length-decode--decode-segm'."))
  (let*
      ((segm-len (run-length-decode--get-segm-length segm))
       (segm-char (seq-elt segm (1- (length segm)))))
    (make-string segm-len segm-char)))


(defun run-length-decode--get-segm-length (segm)
  "Calculate the number of repeats for a run-length encoded segment.
Take a string SEGM as input that is assumed to possibly start
with a series of digits, and have a single trailing nondigit
character.  Return a natural number that is either 1 if there are
no leading digits, or is the value of the number represented by
the leading digits in the string."
  (unless
      (and (stringp segm)
           (string-match-p "^\\([[:digit:]]*\\)?[^[:digit:]]$" segm))
    ("Invalid inputs to `run-length-decode--get-segm-length'."))
  (if (string-match-p "^[[:digit:]]" segm)
      (string-to-number segm)
    1))


(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
