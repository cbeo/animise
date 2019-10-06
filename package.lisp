;;;; package.lisp

(defpackage #:animise
  (:use #:cl #:lettuce)
  (:import-from #:trivia #:match)
  (:export

   ;; TWEEN CLASSES
   #:tween
   #:tween-seq
   #:tween-group

   ;; TWEEN PROTOCOL
   #:start-time
   #:duration
   #:loop-mode
   #:run-tween

   ;; TWEEN FUNCTIONS & Macros
   #:tween-finished-p
   #:in-sequence
   #:end-time
   #:add-to-group
   #:as-group
   #:pause
   #:animate
   #:animating
   #:sequencing
   #:pausing
   #:grouping


   ;; EASING FUNCTIONS
   #:bounce-out
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
