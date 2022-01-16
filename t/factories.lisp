(defpackage factory-alien/test.factories
  (:local-nicknames (:factories :factory-alien.factories))
  (:use :cl :parachute))

(in-package :factory-alien/test.factories)

(define-test id-field-p
  :serial nil

  (false (factories::id-field-p "P")
         "Strings shorter than \"-ID\" don't error.")
  (true (factories::id-field-p "POTATO-ID")
        "Substantial strings ending in \"-ID\" are id fields.")
  (false (factories::id-field-p "-ID")
         "Only the suffix \"-ID\" isn't an id field.")
  (true (factories::id-field-p "A-ID")
        "At least one character before the \"-ID\" suffix is an id field."))
