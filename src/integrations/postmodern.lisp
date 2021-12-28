(defpackage factory-alien/postmodern
  (:local-nicknames (:pomo :postmodern))
  (:use :cl))

(in-package :factory-alien/postmodern)

(defmethod factory-alien:build :around (factory &rest args)
  (let ((dao (call-next-method)))
    (pomo:save-dao dao)
    dao))
