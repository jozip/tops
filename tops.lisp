;;;; tops.lisp -- Main
;;;;
;;;; TOPS, the Tiny Object Prototype System
;;;;
;;;; Copyright (c) 2014, Johan Persson <johan.z.persson@gmail.com>
;;;; 
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package #:tops)

#|
First off, everything is a prototype. What would constitute as a prototype in
Javascript is called parents, for here we have support for multiple
inheritance.
"Ugh," I hear you groan, but bear with me, it's a pretty sane implementation.
Parents reside in a list and is prioritized from left to right. In other words
can properties be resolved by a simple breadth-first traversal of the parents
graph.
Methods are just ordinary defuns that make use of the special variable this to
refer to the current prototype. This means that functions and prototypes are
not bound together, but can readily be swapped out as long as duck-properties
are intact.
|#

(defvar this (extend ())
  "Null context")

(defclass prototype ()
  ((tag :initarg :tag
        :reader tag)
   (parents :initarg :parents
            :accessor parents)
   (properties :initarg :properties
               :accessor properties))
  (:default-initargs
    :parents    nil
    :properties (make-hash-table)
    :tag        (gensym))
  (:documentation
    "The prototype class"))

(defmacro extend ((&rest parents) &body propdefs)
  "Extends a prototype"
  (let ((proto (gensym)))
    `(let ((,proto (make-instance 'prototype)))
       ,@(loop for prop in propdefs
               collect `(setf (gethash ',(car prop) (properties ,proto))
                              ,(cadr prop)))
       ,@(loop for parent in (reverse parents)
               collect `(when (typep ,parent 'prototype)
                          (push ,parent (parents ,proto))))
       ,proto)))

(defun print-assoc (stream arg colonp atsignp)
  "Helper for printing assocs"
  (declare (ignore colonp atsignp))
  (format stream "~A: ~A" (car arg) (cdr arg)))

(defmethod print-object ((object prototype) stream)
  "The print-object implementation of a prototype object"
  (format stream "#<PROTOTYPE ~A ~A {~{~/print-assoc/~^, ~}}>"
          (symbol-name (tag object))
          (mapcar (lambda (parent) (symbol-name (tag parent))) (parents object))
          (hash-table-alist (properties object))))

(defmacro pget (proto key)
  "Simple property getter"
  `(gethash ,key (properties ,proto)))

(defmacro bfs-walker ((tree test next) &body body)
  "Generalized anaphoric breadth-first walker.
   `tree` is the structure to traverse.
   `test` is the test for when to execute `body`.
   `next` is a function which designates the next step in the traversal.
   `body` is an ordinary function body.
   Note: this macro exposes a variable `this` which represents the current subtree."
  (let ((bfs (gensym)))
    `(labels ((,bfs (this)
                (cond ((null this) nil)
                      ((funcall ,test this)
                       ,@body
                       (,bfs (funcall ,next this)))
                      (t (,bfs (append (cdr this)
                                       (car this)))))))
       (,bfs ,tree))))

(defmacro prototype-walker ((proto) &body body)
  "Walker for the parent graph"
  `(bfs-walker (,proto (lambda (p) (typep p 'prototype)) #'parents) ,@body))

(defun property (proto key)
  "Get property of prototype"
  (prototype-walker (proto)
    (multiple-value-bind (value present-p) (pget this key)
      (when present-p
        (return-from property value)))))

(defsetf property (proto key) (value)
         "Set property of prototype"
         `(setf (pget ,proto ,key) ,value))

(defmacro with-prototype ((proto) &body body)
  "Special prototype environment"
  `(let ((this ,proto))
     (declare (special this))
     ,@body))

