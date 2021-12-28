(defpackage factory-alien/test
  (:use :cl :parachute))

(in-package :factory-alien/test)

(deftest test-factory-alien
  (ok (eq 1 1) "testing works"))
