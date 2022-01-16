;;;; factory-alien.asd

(defsystem factory-alien
  :description "Factory alien is a library for generating fixture data for testing applications."
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license  "MIT"
  :version "0.0.2"
  :depends-on ("alexandria"
               "closer-mop")
  :pathname "src"
  :components ((:file "sequences")
               (:file "traits")
               (:file "factories")
               (:file "factory-alien"))
  :homepage "https://fisxoj.github.io/factory-alien/"
  :in-order-to ((test-op (test-op factory-alien/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))


(defsystem factory-alien/test
  :depends-on ("factory-alien"
               "parachute")
  :pathname "t"
  :components ((:file "factories")
               (:file "factory-alien"))
  :perform (test-op (op c)
                    (declare (ignore op))
                    (uiop:symbol-call :parachute :test
                                      '(:factory-alien/test
                                        :factory-alien/test.factories))))

(defsystem factory-alien/postmodern
  :depends-on ("factory-alien"
               "postmodern")
  :description "Integration to create and persist dao objects with postmodern."
  :pathname "src/integrations/"
  :components ((:file "postmodern")))
