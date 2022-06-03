(defpackage factory-alien
  (:use :cl)
  (:local-nicknames (:factories #:factory-alien.factories)
                    (:traits #:factory-alien.traits))
  (:import-from #:factory-alien.factories
                #:build
                #:build-many
                #:define-factory)
  (:import-from #:factory-alien.sequences
                #:define-sequence)
  (:export
   #:define-factory
   #:define-sequence
   #:build
   #:build-many))

(in-package :factory-alien)
