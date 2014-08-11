;;;; tops.asd

(asdf:defsystem #:tops
  :serial t
  :description "Tiny Object Property System"
  :author "Johan Persson <johan.z.persson@gmail.com>"
  :license "ISC"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "tops")))

