;;; -*- lexical-binding: t -*-
;;; robot-name.el --- Robot Name (exercism)

;;; Commentary:
;;;
;;; NOTE THAT THIS SOLUTION FAILS THE TEST `name-can-be-reset'.

;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

(defvar robot--existing-names (make-hash-table :test 'equal))


(defun build-robot ()
  (let* ((name (robot--assign-name)))
    (build-robot--impl name)))


(defun robot-name (robot)
  (cdr (assoc 'name robot)))


(defun reset-robot (robot)
  (unless (robotp robot)
    (error "The input for ROBOT must be a robot."))
  (let* ((name (robot--assign-name)))
    (cons
     (cons 'name name)
     robot)))


(defun robotp (x)
  (and
   (listp x)
   (assoc 'name x)))


(defun robot--assign-name ()
  (let* ((name (robot--create-random-name-unique 0 100)))
    (robot--insert-name name)
    name))


(defun robot--create-random-name ()
  (let*
      ((uppercase-shift 65)
       (num-shift 48)
       (char1 (+ (random 27) uppercase-shift))
       (char2 (+ (random 27) uppercase-shift))
       (digit1 (+ (random 10) num-shift))
       (digit2 (+ (random 10) num-shift))
       (digit3 (+ (random 10) num-shift)))
    (string char1 char2 digit1 digit2 digit3)))


(defun robot--create-random-name-unique (attempt-num attempt-max)
  (unless
      (and
       (wholenump attempt-num)
       (wholenump attempt-max))
    (error "ATTEMPT-NUM and ATTEMPT-MAX must both be nonnegative integers."))
  (unless (< attempt-num attempt-max)
    (error "Unable to generate a unique random name."))
  (let* ((name (robot--create-random-name)))
    (if (robot--check-name-exists name)
        (robot--create-random-name-unique (1+ attempt-num) attempt-max)
      name)))


(defun robot--check-name-exists (name)
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (gethash name robot--existing-names))


(defun robot--insert-name (name)
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (puthash name t robot--existing-names))


(defun build-robot--impl (name)
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (list (cons 'name name)))


(provide 'robot-name)
;;; robot-name.el ends here
