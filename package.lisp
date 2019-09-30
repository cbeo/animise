;;;; package.lisp

(defpackage #:animise
  (:use #:cl)
  (:export

   ;; TWEENS
   #:tween
   #:make-tween
   #:end-time
   #:tween-finished-p

   ;; EASING FUNCTIONS
   #:bouce-out
   #:cubic-in
   #:cubic-in-out
   #:cubic-out
   #:elastic-out
   #:linear
   #:mirror-bounce-out
   #:mirror-cubic-in
   #:mirror-cubic-in-out
   #:mirror-cubic-out
   #:mirror-elastic-out
   #:mirror-linear
   #:mirror-quad-in
   #:mirror-quad-in-out
   #:mirror-quad-out
   #:mirror-sinusoidal-in
   #:mirror-sinusoidal-in-out
   #:mirror-sinusoidal-out
   #:quad-in
   #:quad-in-out
   #:quad-out
   #:sinusoidal-in
   #:sinusoidal-in-out
   #:sinusoidal-out
   ))
