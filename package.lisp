;;;; package.lisp

(defpackage #:animise
  (:use #:cl)
  (:export
   #:linear
   #:mirror-linear
   #:quad-in
   #:quad-out
   #:mirror-quad-in
   #:mirror-quad-out
   #:quad-in-out
   #:mirror-quad-in-out
   #:cubic-in
   #:cubic-out
   #:mirror-cubic-in
   #:mirror-cubic-out
   #:cubic-in-out
   #:mirror-cubic-in-out
   #:sinusoidal-in
   #:mirror-sinusoidal-in
   #:sinusoidal-out
   #:mirror-sinusoidal-out
   #:sinusoidal-in-out
   #:mirror-sinusoidal-in-out
   #:elastic-out
   #:mirror-elastic-out
   #:bouce-out
   #:mirror-bounce-out
   ))
