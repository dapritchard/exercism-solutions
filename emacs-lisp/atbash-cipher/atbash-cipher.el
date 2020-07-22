;;; -*- lexical-binding: t -*-
;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:

;; Note that `seq-split' could be replaced with `seq-partition', (along with
;; `mapconcat' to insert the separating spaces).


;;; Code:

(defun encode (plaintext)
  "Encode PLAINTEXT to atbash-cipher encoding.
The encoding is based on the char-table CIPHER--MAP."
  (unless (stringp plaintext)
    (error "PLAINTEXT must be a string."))
  (let*
      ((plaintext-alnum (replace-regexp-in-string "[^[:alnum:]]" "" plaintext))
       (plaintext-alnum-lower (downcase plaintext-alnum))
       (transl (translate-string plaintext-alnum-lower cipher--map))
       (transl-split-list (seq-split transl 5 " ")))
    (apply #'concat transl-split-list)))


(defun translate-string (from-string transl-char-table)
  "Translate a string based on a char-table.
A new string is created based on the mappings of the characters
in FROM-STRING according to TRANSL-CHAR-TABLE."
  (unless
      (and
       (stringp from-string)
       (char-table-p transl-char-table)))
  (let*
      ((from-string-len (length from-string))
       (to-string (make-string from-string-len 0)))
    (dotimes (i from-string-len)
      (let*
          ((from-char (aref from-string i))
           (to-char (aref transl-char-table from-char)))
        (aset to-string i to-char)))
    to-string))


(defun seq-split (s n sep)
  "Split a sequence into a list with separators.
Takes a sequence S and splits it into a list with sub-sequences
of length N (except possibly the last element), and separated by
elements equal to SEP."
  (unless
      (and
       (sequencep s)
       (wholenump n)
       (> n 0))
    (error "Invalid inputs."))
  (cond
   ((null s) ())
   ((<= (length s) n) (list s))
   (t (cons
       (seq-take s n)
       (cons
        sep
        (seq-split (seq-drop s n) n sep))))))


;; map of characters to their atbash cipher translations
(defvar cipher--map (make-char-table 'cipher))

;; map the letters to their atbash cipher translations
(dotimes (i 26)
  (aset cipher--map (+ ?a i) (- ?z i)))

;; map the integers to themselves
(dotimes (i 10)
  (let* ((k (+ ?0 i)))
    (aset cipher--map k k)))


(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
