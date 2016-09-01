(in-package :cl-user)

(defpackage keyword-dispatch
  (:use :cl)
  (:export #:keyword-dispatch-function
           #:not-implemented
           #:add-specializer
           #:define-keyword-generic
           #:define-keyword-method
           #:keyword-dispatch-function-specializers)
  (:export #:keyword-function-not-implemented
           #:keyword-function-not-implemented-arguments
           #:keyword-function-not-implemented-function))

(in-package :keyword-dispatch)

(defclass keyword-dispatch-function ()
  ((specializers
    :initform nil
    :accessor keyword-dispatch-function-specializers
    :documentation "Internal structure to find specializers"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Represents an internal structure of a function that can be dispatch based
on what keyword arguments were provided"))

(defmethod initialize-instance :after ((object keyword-dispatch-function) &key)
  (with-slots (specializers) object
    (closer-mop:set-funcallable-instance-function
     object
     #'(lambda (&rest args)
         (find-function specializers args (not-implemented object))))))

(defun find-function (specializers args &optional found)
  "Finds the best specialized function in SPECIALIZERS structure
(see KEYWORD-DISPATCH-FUNCTION slot SPECIALIZERS) based on the
list of keyword arguments ARGS.
FOUND is used internally to indicate if a specialized function was found"
  (let ((not-found (gensym)))
   (loop for (key function subspecs) in specializers
      if (not (eq (getf args key not-found) not-found))
      return (find-function subspecs args function)
      end
      finally (return (apply found args)))))


(define-condition keyword-function-not-implemented (error)
  ((function
    :initarg :function
    :reader keyword-function-not-implemented-function
    :documentation "Keyword dispatch function that was applied")
   (arguments
    :initarg :arguments
    :reader keyword-function-not-implemented-arguments
    :documentation "Arguments to which function was applied"))
  (:documentation
   "Error condition raised when dispatch cannot find\
specializer for the arguments"))

(defun not-implemented (function)
  "Placeholder function to fill in vacant slots
in SPECIALIZERS structure if a particular specialized function was not found.
This occasion may arise when function was called with less keywords than what
it was specialized on"
  (lambda (&rest args)
    (error 'keyword-function-not-implemented
           :function function
           :arguments args)))

(defun add-specializer (dispatcher new-specializers-list function)
  "Adds a specialized FUNCTION to a keyword-generic DISPATCHER (instance of
KEYWORD-DISPATCH-FUNCTION) based on the list of keyword NEW-SPECIALIZERS-LIST"
  (with-slots (specializers) dispatcher
    (labels ((helper (new-specs specializers)
               (let* ((key (first new-specs))
                      (entry (assoc key specializers)))
                 (if (null (cdr new-specs))
                     (cond (entry
                            (setf (second entry) function)
                            specializers)
                           (t (cons (list key function nil) specializers)))
                     (cond (entry
                            (setf (third entry)
                                  (helper (rest new-specs) (third entry)))
                            specializers)
                           (t (cons (list key
                                          (not-implemented dispatcher)
                                          (helper (rest new-specs) nil))
                                    specializers)))))))
      (setf specializers (helper new-specializers-list specializers))
      dispatcher)))

(defmacro define-keyword-generic (name &rest args)
  "Defines new keyword-generic function NAME.
ARGS provide extra parameters. For example, (:DOCUMENTATION \"Docs\")"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (fdefinition ',name) (make-instance 'keyword-dispatch-function))
     ,(when (assoc :documentation args)
            `(setf (documentation (function ,name) t)
                   ,(second (assoc :documentation args))))))

(defmacro define-keyword-method (name required-keywords lambda-list &body body)
  "Defines a specialized method for keyword-generic function NAME
REQUIRED-KEYWORS is the list of keywords required for the specialization
LAMBDA-LIST is the list of arguments of the specialized function
BODY is the function body
Limitation: the order in REQUIRED-KEYWORDS matters - the first items have higher
priority"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (add-specializer (function ,name)
                      ',required-keywords
                      (lambda (,@lambda-list)
                        ,@body))))


;; (define-keyword-generic foo
;;     (:documentation "Some function foo"))

;; (define-keyword-method foo (:x :y) (&rest args &key x y &allow-other-keys)
;;   (+ x y))

;; (define-keyword-method foo (:x) (&key x &allow-other-keys)
;;   x)

;; (foo :x 1 :z 2 :y 5)



;; (defvar *f* (make-instance 'keyword-dispatch-function))

;; (setf (fdefinition 'ff) *f*)

;; (ff :x 1 :y 2)

;; (add-specializer *f* '(:x :y) (lambda (&rest args &key x y) (+ x y)))

;; (find-function (slot-value *f* 'specializers) '(:x 1 :y 2))

;; (funcall *f* :x 1 :y 2)
