(in-package :cl-user)

(defpackage keyword-dispatch-test
  (:use :cl
        :keyword-dispatch
        :prove))

(in-package :keyword-dispatch-test)

;; NOTE: To run this test file, execute `(asdf:test-system :keyword-dispatch)' in your Lisp.

(plan 5)

(fmakunbound 'kw-add)

(define-keyword-generic kw-add
    (:documentation "Function to test"))

(is-error (kw-add :x 10) 'keyword-function-not-implemented
          "No specializers throws an error")

(define-keyword-method kw-add (:x) (&key x &allow-other-keys)
  x)

(is (kw-add :x 10) 10
    "One specializer: expected result")


(is (kw-add :x 10 :y 20) 10
    "One specializer only: second argument is ignored")

(define-keyword-method kw-add (:x :y) (&key x y &allow-other-keys)
  (+ x y))

(is (kw-add :x 10 :y 20 :z 40) 30
    "Two specializers, other arguments are ignored")

(is-error (kw-add :y 20) 'keyword-function-not-implemented
          "Sub-arg specializer cannot catch it directly")

(finalize)
