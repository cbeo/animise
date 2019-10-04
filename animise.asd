;;;; animise.asd

(asdf:defsystem #:animise
  :description "General Purpose Tweens"
  :author "<thegoofist@protonmail.com>"
  :license  "AGPLv3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:lettuce #:trivia)
  :components ((:file "package")
               (:file "easing")
               (:file "animise")
               (:file "tweening")))
