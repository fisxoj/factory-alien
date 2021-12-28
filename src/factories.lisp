(defpackage factory-alien.factories
  (:use :cl)
  (:local-nicknames (:traits :factory-alien.traits))
  (:shadow #:slot-value)
  (:export
   #:find-factory
   #:slot-value))

(in-package :factory-alien.factories)

(defparameter *factories* (make-hash-table)
  "A map of factory names (as keywords) to instances of :class:`factory`.")

(define-condition no-such-factory ()
  ((factory-name :initarg :factory-name))
  (:report (lambda (condition stream)
             (with-slots (factory-name) condition
               (format stream "No such factory ~A exists." factory-name)))))

(defun find-factory (name-or-class &optional (errorp t))
  "Takes a symbol or class and, if a symbol is given, calls :function:`find-class` on it."

  (etypecase name-or-class
    (keyword (or (gethash name-or-class *factories*)
                 (when errorp
                   (error 'no-such-factory :factory-name name-or-class))))
    (factory name-or-class)))

(defmacro define-factory (name parent-factories &body body-and-options)
  (let ((name (alexandria:make-keyword name))
        (traits (traits:process-trait-definitions
                 (list* (cons t (first body-and-options))
                        (loop :for option :in (cdr body-and-options)
                              :when (eq (car option) :trait)
                                :collect (cons (second option) (cddr option))))))
        (instantiable-class (second (find :model (cdr body-and-options) :key 'first))))
    `(setf (gethash ,name *factories*)
           (make-instance 'factory
                          :traits ',traits
                          :parent-factories ,parent-factories
                          :instantiable-class ',instantiable-class))))

(defclass factory ()
  ((traits :initarg :traits
           :reader traits
           :documentation "Mapping of traits ")
   (parent-factories :initarg :parent-factories
                     :reader parent-factories
                     :documentation "List of parent factories that should be run and merged into instances of this one.")
   (instantiable-class :initarg :instantiable-class
                       :reader instantiable-class
                       :documentation "The class to make an instance of."))
  (:documentation "Singleton class for each factory that holds things like the sequences used to create the instances of the class that the factory is a proxy for."))

(defun build (factory traits &rest initargs)
  (let* ((factory (find-factory factory))
         (parent-traits (mapcar (alexandria:compose #'get-default-trait #'find-factory) (parent-factories factory))))
    (loop :with object := (make-instance (instantiable-class factory))
          :for (name . value) :in (traits:collect-slot-values (append parent-traits (traits factory)) traits initargs)
          :do (setf (slot-value factory object name) value)
          :finally (return object))))

(defun get-default-trait (factory)
  (assoc t (traits factory)))

(defun slot-value (factory object name)
  (let ((slot (find name (closer-mop:class-slots (find-class (instantiable-class factory)))
                    :key #'closer-mop:slot-definition-name
                    :test #'string-equal)))
    (cl:slot-value object (closer-mop:slot-definition-name slot))))

(defun (setf slot-value) (value factory object name)
  (let ((slot (find name (closer-mop:class-slots (find-class (instantiable-class factory)))
                    :key #'closer-mop:slot-definition-name
                    :test #'string-equal)))
    (setf (cl:slot-value object (closer-mop:slot-definition-name slot)) value)))
