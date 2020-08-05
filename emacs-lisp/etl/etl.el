;;; -*- lexical-binding: t -*-
;;; etl.el --- etl Exercise (exercism)

;;; Commentary:

;;; Code:

(defun etl (val-letter-tbl)
  "Create a new hash table with an isomorphic data representation.
Take a hash table VAL-LETTER-TBL as input.  The hash table is
expected to have integer values between 1 and 10 as keys, and
lists of single-letter strings as values.  Return a hash table
with (a downcased version of) each of the strings in the original
table as keys, and their corresponding key from the original
table as their value."
  (unless (hash-table-p val-letter-tbl)
    (error "VAL-LETTER-HT must be a hash table."))
  (let*
      ((letter-val-tbl (make-hash-table :test #'equal))
       (key-list (etl--get-hash-keys val-letter-tbl))
       (insert-key-list
        (lambda (value)
          (etl--insert-key-list letter-val-tbl
                                (gethash value val-letter-tbl)
                                value))))
    (seq-do insert-key-list key-list)
    letter-val-tbl))


(defun etl--insert-key-list (letter-val-tbl key-list value)
  "Insert keys into a hash table with a common value.
Take a hash table TBL, a list KEY-LIST, and an arbitrary object
VALUE as inputs.  For each element in KEY-LIST, insert an entry
into TBL with the element from KEY-LIST as the key and VALUE as
the value.  Return the modified version of LETTER-VAL-TBL."
  (unless
      (and (integerp value) (<= 1 value) (<= value 10))
    (error (format "The key '%s' must be an integer from 1 to 10."
                   (prin1-to-string value))))
  (dolist (key key-list)
    (unless
        (and (stringp key)
             (string-match-p "^[[:alpha:]$]" key))
      (error (format "Value '%s' must be a string with a single letter."
                     (prin1-to-string key))))
    (puthash (downcase key) value letter-val-tbl)))


(defun etl--get-hash-keys (tbl)
  "Obtain the keys from a hash table in a list form."
  (let*
      ((key-list)
       (extract-key
        (lambda (key _value)
          (push key key-list))))
    (maphash extract-key tbl)
    key-list))


(provide 'etl)
;;; etl.el ends here
