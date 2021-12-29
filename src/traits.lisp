(defpackage factory-alien.traits
  (:local-nicknames (:sequences :factory-alien.sequences))
  (:use :cl)
  (:export
   #:process-trait-definitions
   #:collect-slot-values))

(in-package :factory-alien.traits)

(defun resolve-traits (trait-definition)
  "Given an alist of (slot-name . funcallable), call every funcallable and return a new alist of (name . value)."
  (loop :for (name . funcallable) :in trait-definition
        :collect (cons name (funcall funcallable))))

(defun merge-traits (a b)
  "If a field exists in b, it takes precedence over the field in a, but all fields in a OR b should end up in the final alist. Modifies b in-place for PERFORMANCE."

  (loop :for (name . funcallable) :in a
        :unless (assoc name b)
          :do (setf b (cons (cons name funcallable) b))
        :finally (return b)))

(defun collect-slot-values (trait-definitions traits initargs)
  (resolve-traits
   (reduce #'merge-traits
           (mapcar #'cdr
                   (remove-if-not (lambda (trait-name)
                                    (or (eq trait-name t)
                                        (member trait-name traits)))
                                  trait-definitions
                                  :key #'car))
           :initial-value (loop :for (name value) :on initargs :by #'cddr :collecting (cons name (lambda () value))))))

(defgeneric make-trait-initializer (type body &rest options)
  (:method ((type (eql :sequence)) (sequence-name symbol) &rest options)
    (declare (ignore options))
    (lambda () (sequences:call-sequence sequence-name)))
  (:method ((type (eql :sequence)) lambda &rest options)
    (declare (ignore options))
    (let ((name (sequences:make-anonymous-sequence (eval lambda))))
      (lambda () (sequences:call-sequence name))))
  (:method ((type (eql :initform)) value &rest options)
    (declare (ignore options))
    (lambda () value))
  (:method ((type (eql :factory)) factory &rest options)
    ;; FIXME: Is this the best way or do I need to restructure things?
    (destructuring-bind (&optional traits initargs) options
      (lambda ()
        (apply (find-symbol "BUILD" "FACTORY-ALIEN") factory traits initargs)))))

(defun process-trait-definitions (traits)
  "Creates an alist of (trait-name . slot-initializer-alist), where slot-initializer-alist contains an alist mapping slot name keyword to a function that provides the value for that slot."
  (loop :for (trait . definitions) :in traits
        :collect (cons trait (loop :for (name . arguments) :in definitions
                                   :collecting (cons name (apply #'make-trait-initializer arguments))))))
