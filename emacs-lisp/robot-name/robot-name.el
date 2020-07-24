;;; -*- lexical-binding: t -*-
;;; robot-name.el --- Robot Name (exercism)

;;; Commentary:

;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

;; create a hash table storing the robots' data
(defvar robot--robot-data (make-hash-table :test 'equal))


(defun build-robot ()
  "Create a robot."
  (let* ((name (robot--create-random-name-unique)))
    (robot--build-robot-impl name)))


(defun robot-name (robot)
  "Obtain the robot's name.
Take a robot ROBOT as input, and return a string providing the
robot's name as output."
  (unless (robotp robot)
    (error "The input for ROBOT must be a robot."))
  (let*
      ((robot-data (robot--get-robot-data robot)))
    (alist-get 'name robot-data)))


(defun reset-robot (robot)
  "Give a robot a new name.
Take a robot ROBOT and return a string providing the robot's new
name.  The data associated with the robot is modified so that the
change is persistent."
  (unless (robotp robot)
    (error "The input for ROBOT must be a robot."))
  (let*
      ((robot-data (robot--get-robot-data robot))
       (new-name (robot--create-random-name-unique)))
    (robot--mark-name-as-taken new-name)
    (robot--update-name robot-data new-name)))


(defun robotp (x)
  "Check whether an object is a robot."
  (and
   (listp x)
   (assoc 'hash-key x)))


(defun robot--build-robot-impl (name)
  "`build-robot' internal version.
Take a string NAME as input and return a robot."
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (let*
      ((robot-data (robot--create-robot-data name)))
    (robot--insert-data name robot-data)
    (list (cons 'hash-key name))))


(defun robot--create-robot-data (name)
  "Create the data associated with a robot.
Take a string NAME as input and return an alist with at least a
key 'name."
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (list (cons 'name name)))


(defun robot--insert-data (name robot-data)
  "Insert robot data into a hash table.
Take a string NAME and robot data ROBOT-DATA as inputs, and
return the input robot data."
  (unless
      (and
       (stringp name)
       (robot--robot-data-p robot-data))
    (error "Invalid inputs."))
  (puthash name robot-data robot--robot-data))


(defun robot--robot-data-p (x)
  "Check whether an object is a robot."
  (and
   (listp x)
   (assoc 'name x)))


(defun robot--get-robot-data (robot)
  "Obtain the data associated with a robot."
  (unless (robotp robot)
    (error "The input to ROBOT must be a robot."))
  (let*
      ((hash-key (alist-get 'hash-key robot))
       (robot-data (gethash hash-key robot--robot-data)))
    (unless robot-data
      (error "Unable to find the data associated with ROBOT."))
    robot-data))


(defun robot--update-name (robot-data new-name)
  "Update the name in the robot data.
Take a robot data object ROBOT-DATA and string NEW-NAME, and
return a modified version version of robot data where the value
associated with 'name is replaced by NEW-NAME."
  (setcdr (assoc 'name robot-data) new-name))


(defun  robot--create-random-name-unique ()
  "Create a unique name at random.
Attempt to create a unique name at random, according to the robot
name constraints.  If 100 names are created at random such that
each proposed name is already taken, then an error is thrown."
  (robot--create-random-name-unique-impl 0 100))


(defun robot--create-random-name-unique-impl (attempt-num attempt-max)
  "Internal version of `robot--create-random-name-unique'.
Take integers ATTEMPT-NUM and ATTEMPT-MAX as inputs and attempt
to create a unique name at random, according to the robot name
constraints.  If either (i) ATTEMPT-NUM >= ATTEMPT-MAX, or
ATTEMPT-MAX - ATTEMPT-NUM names are created at random such that
each proposed name is already taken, then an error is thrown."
  (unless
      (and
       (integerp attempt-num)
       (integerp attempt-max))
    (error "ATTEMPT-NUM and ATTEMPT-MAX must both be nonnegative integers."))
  (unless (< attempt-num attempt-max)
    (error "Unable to generate a unique random name."))
  (let*
      ((name (robot--create-random-name)))
    (if (robot--check-name-exists name)
        (robot--create-random-name-unique (1+ attempt-num) attempt-max)
      name)))


(defun robot--create-random-name ()
  "Create a name at random conforming to the robot name constraints."
  (let*
      ;; the letter ?A is ASCII character 65, and the number ?0 is ASCII
      ;; character 48
      ((uppercase-shift 65)
       (num-shift 48)
       (char1 (+ (random 26) uppercase-shift))
       (char2 (+ (random 26) uppercase-shift))
       (digit1 (+ (random 10) num-shift))
       (digit2 (+ (random 10) num-shift))
       (digit3 (+ (random 10) num-shift)))
    (string char1 char2 digit1 digit2 digit3)))


(defun robot--check-name-exists (name)
  "Check if a proposed robot name already exists.
Take a string NAME as input, and return nil if the name is
already taken, or non-nil otherwise."
  (unless (stringp name)
    (error "The input to NAME must be a string."))
  (gethash name robot--robot-data))


(defun robot--mark-name-as-taken (name)
  "Mark a robot name as taken."
  (puthash name t robot--robot-data))


(provide 'robot-name)
;;; robot-name.el ends here
