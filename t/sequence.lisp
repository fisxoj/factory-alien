(defpackage factory-alien/test.sequence
  (:local-nicknames (:sequence :factory-alien.sequence))
  (:use :cl :parachute))

(in-package :factory-alien/test.sequence)

(define-test define-sequence
  :compile-at :execute
  :fix (sequence::*sequence-state*)

  (fail (eval '(sequence:define-sequence 5 (n) (1+ n)))
      'type-error
      "Name can't be a number.")

  (fail (sequence:define-sequence potato (n) (1+ n))
      'type-error
      "Name can't be a keyword."))

(define-test call-sequence
  :serial t
  :fix (sequence::*sequence-state*)

  (sequence:define-sequence :email (n)
    (format nil "person~d@example.com" n))

  (is string=
      "person0@example.com"
      (sequence:call-sequence :email))

  (is string=
      "person1@example.com"
      (sequence:call-sequence :email)))

(define-test call-sequence-with-initial-value
  :serial t
  :fix (sequence::*sequence-state*)

  (sequence:define-sequence (:user-id :initial-value 1000) (n)
    n)

  (is =
      1000
      (sequence:call-sequence :user-id)
      "The first call gets the initial value."))
