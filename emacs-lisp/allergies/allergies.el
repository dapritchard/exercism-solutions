;;; -*- lexical-binding: t -*-
;;; allergies.el --- Allergies Exercise (exercism)

;;; Commentary:

;;; Code:

(defun allergen-list (n)
  "Return the list of allergens encoded by N"
  (unless (natnump n)
    (error "The input for 'n' must be a natural number."))
  (let*
      ((allergies-list
        (list
         "eggs"
         "peanuts"
         "shellfish"
         "strawberries"
         "tomatoes"
         "chocolate"
         "pollen"
         "cats")))
    (reverse (allergen-list--impl n allergies-list ()))))


(defun allergen-list--impl (n in-list out-list)
  "Return a list of allergens.
Takes a nonnegative integer N and lists IN-LIST and OUT-LIST as
inputs, and for each '1' bit in the binary representation of N,
prepends the corresponding element of IN-LIST to OUT-LIST in
reverse order, and where the least significant bits correspond to
the start of IN-LIST.  If the length of IN-LIST isn't long enough
to accommodate all of the '1' bits in the N, then the higher-end
bits are ignored."
  (unless
      (and
       (natnump n)
       (listp in-list)
       (listp out-list))
    (error "Invalid inputs."))
  ;; stop if the number reaches 0 or we've exhausted the number of bits that
  ;; have meaning
  (if (or (zerop n) (null in-list))
      out-list
    (let*
        ((bit (% n 2))
         (quotient (/ n 2))
         (curr-item (car in-list))
         (in-list-next (cdr in-list)))
      ;; if the current bit is zero then discard CURR-ITEM, otherwise prepend it
      ;; to the result of combining the higher-end bits
      (if (zerop bit)
          (allergen-list--impl quotient in-list-next out-list)
        (allergen-list--impl quotient in-list-next (cons curr-item out-list))))))


(defun allergic-to-p (n item)
  "Check whether an item is among list of allergens encoded by N."
  (unless
      (and
       (natnump n)
       (stringp item))
    (error "Invalid inputs."))
  (let*
      ((allergens-for-n-list (allergen-list n)))
    (member item allergens-for-n-list)))


(provide 'allergies)
;;; allergies.el ends here
