;;;; animise.asd

(asdf:defsystem #:animise
  :description "Describe animise here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lettuce)
  :components ((:file "package")
               (:file "animise")))
