;;;; factory-alien.asd

(defsystem factory-alien
  :description "Describe factory-alien here"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on ("access"
               "alexandria"
               "closer-mop")
  :pathname "src"
  :components ((:file "sequences")
               (:file "factories")
               (:file "traits")
               (:file "factory-alien"))
  :homepage "https://fisxoj.github.io/factory-alien/"
  :in-order-to ((test-op (test-op factory-alien/test)))
  :long-description #.(uiop:read-file-string #P"README.rst"))


(defsystem factory-alien/test
  :depends-on ("factory-alien"
               "parachute")
  :pathname "t"
  :components ((:file "factory-alien"))
  :perform (test-op (op c)
                    (declare (ignore op))
                    (uiop:symbol-call :parachute :test c)))

(defsystem factory-alien/postmodern
  :depends-on ("factory-alien"
               "postmodern")
  :description "Integration to create and persist dao objects with postmodern."
  :pathname "src/integrations/"
  :components ((:file "postmodern")))
