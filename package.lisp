;;;; package.lisp

(defpackage #:tops
  (:use #:cl
        #:alexandria)
  (:export #:extend
           #:prototype-walker
           #:property
           #:with-prototype
           #:tag
           #:parents
           #:properties
           #:this))

