#|
  This file is a part of keyword-dispatch project.
  Copyright (c) 2016 Alexey Cherkaev
|#

#|
  Author: Alexey Cherkaev
|#

(in-package :cl-user)
(defpackage keyword-dispatch-asd
  (:use :cl :asdf))
(in-package :keyword-dispatch-asd)

(defsystem keyword-dispatch
  :version "0.1"
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (:closer-mop)
  :components ((:module "src"
                :components
                ((:file "keyword-dispatch"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op keyword-dispatch-test))))
