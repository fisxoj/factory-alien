(defpackage factory-alien/test
  (:use :cl :parachute))

(in-package :factory-alien/test)

(defclass dog ()
  ((name :type string)
   (age :type (integer 0))
   (spayed-p :type boolean)))

(factory-alien:define-sequence :name (n)
  (nth n (alexandria:circular-list "Rover" "Spot" "Fido" "Scamp")))

(factory-alien:define-factory :dog ()
  ((:name :sequence :name)
   (:age :sequence (lambda (n) (1+ n)))
   (:spayed-p :initform nil))
  (:trait :spayed
          (:spayed-p :initform t))
  (:model dog))

(define-test whole-shebang
  :serial nil

  (factory-alien.sequences:reset-sequences)

  (let ((dog1 (factory-alien:build :dog nil))
        (dog2 (factory-alien:build :dog '(:spayed)))
        (dog3 (factory-alien:build :dog nil :name "Reginald")))

    (with-slots (name age spayed-p) dog1
      (is string= "Rover" name)
      (is = 1 age)
      (false spayed-p))

    (with-slots (name age spayed-p) dog2
      (is string= "Spot" name)
      (is = 2 age)
      (true spayed-p))

    ;; Check that overrides work
    (with-slots (name) dog3
      (is string= "Reginald" name))))
