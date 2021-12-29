(defpackage factory-alien/postmodern
  (:local-nicknames (:pomo :postmodern))
  (:use :cl))

(in-package :factory-alien/postmodern)

(defmethod factory-alien:build :around (factory traits &rest args)
  (declare (ignore factory traits args))
  (let ((dao (call-next-method)))
    (when (typep (class-of dao) 'pomo:dao-class)
      (pomo:insert-dao dao))
    dao))
