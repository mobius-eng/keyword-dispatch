#|
  This file is a part of keyword-dispatch project.
  Copyright (c) 2016 Alexey Cherkaev
|#

(in-package :cl-user)
(defpackage keyword-dispatch-test-asd
  (:use :cl :asdf))
(in-package :keyword-dispatch-test-asd)

(defsystem keyword-dispatch-test
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (:keyword-dispatch
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "keyword-dispatch"))))
  :description "Test system for keyword-dispatch"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
