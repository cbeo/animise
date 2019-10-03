;;;; animise.asd

(asdf:defsystem #:animise
  :description "Describe animise here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lettuce #:trivia)
  :components ((:file "package")
               (:file "easing")
               (:file "animise")))
