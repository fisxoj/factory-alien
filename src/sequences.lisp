(defpackage factory-alien.sequences
  (:use :cl)
  (:shadow #:sequence
           #:make-sequence)
  (:export
   #:reset-sequence
   #:reset-sequences
   #:define-sequence
   #:call-sequence
   #:make-anonymous-sequence))

(in-package :factory-alien.sequences)

(defvar *sequence-state* (make-hash-table :test 'equal)
  "The counters for every sequence as a mapping of symbol to an instance of :class:`sequence`.  Named sequences are stored as their keyword symbol, anonymous sequences are given a numbered name by :function:`make-anonymous-sequence`.")

(defvar *anonymous-sequence-counter* 0
  "Like :variable:`*gensym-counter*`, gets used to generate unique names for anonymous sequences.")

(defun reset-sequences ()
  (loop :for key :being :the :hash-key :in *sequence-state*
        :do (reset-sequence key)))

(defun reset-sequence (name)
  (let ((sequence (gethash name *sequence-state*)))
    (setf (sequence-value sequence)
          (sequence-initial-value sequence))))

(defstruct sequence
  (value 0 :type integer)
  (initial-value 0 :type integer)
  (funcallable 'identity :type (or symbol function)))

(defun make-anonymous-sequence (funcallable &key (initial-value 0))
  "Takes a function of one argument and returns the name of the sequence that calls that function."
  (let ((name (alexandria:make-keyword (format nil "SEQUENCE~d" (incf *anonymous-sequence-counter*)))))
    (setf (gethash name *sequence-state*)
          (make-sequence :initial-value initial-value
                         :value initial-value
                         :funcallable funcallable))
    name))

(defmacro define-sequence (name-and-options lambda-list &body body)
  (let* ((name (car (alexandria:ensure-list name-and-options)))
         (options (cdr (alexandria:ensure-list name-and-options)))
         (initial-value (getf options :initial-value 0)))
    (check-type name keyword)

    (alexandria:once-only (initial-value)
      `(setf (gethash ,name *sequence-state*)
             (make-sequence :initial-value ,initial-value
                            :value ,initial-value
                            :funcallable (lambda ,lambda-list
                                           ,@body))))))

(defun call-sequence (name)
  (let ((sequence (gethash name *sequence-state*)))
    (prog1 (funcall (sequence-funcallable sequence) (sequence-value sequence))
      (incf (sequence-value sequence)))))
