(in-package :animise)

(defmacro when-bound (symbol)
  `(when (boundp (quote ,symbol)) ,symbol))

(defvar *duration*)
(defvar *target*)
(defvar *start-time*)

(defmacro sequencing ((&key loop-mode at targeting) &body forms)
  "START and STARTING are synonym keywords, and are used to specify as tart time.

   WITH-TARGET, TARGETING, and TARGET are synonym keywords, used to specify a
   default tween target for all tweens made within the body of this form.

   LOOP-MODE is supplied to the LOOP-MODE slot on the TWEEN-SEQ class that this
   form returns."
  (let ((this-seq (gensym))
        (dyn-vars (append (when targeting
                            `((*target* ,targeting )))
                           (when at 
                             `((*start-time* ,at))))))
    `(let ,dyn-vars
       (let ((,this-seq (in-sequence ,@forms)))
         (setf (loop-mode ,this-seq) ',loop-mode)
         ,this-seq))))


(defun pausing (&key for at)
  "A light wrapper around PAUSE. Is aware of default parameters that may have
   been set in an enclosing form."
  (pause (or for (when-bound *duration*) 0)
         (or at (when-bound *start-time*) 0)))

(defmacro grouping ((&key for) &body forms)
  "Returns a TWEEN-GROUP instance.

   FOR is used to specify a default duration for any tweens defined in this
   body, where applicable."
  (let ((dyn-vars (append (when for 
                            `((*duration* ,for ))))))
    `(let ,dyn-vars
       (as-group ,@forms))))


(defun keyword->ease-fn (name)
  (case name
    ((:bounce-out :bouncing-out) #'bounce-out)
    ((:cubing-in :cubic-in) #'cubic-in)
    ((:cubing-in-out  :cubic-in-out) #'cubic-in-out)
    ((:cubing-out  :cubic-out) #'cubic-out)
    ((:elastic-out :springing-out :spring-out) #'elastic-out)
    ((:linear :linearly :none) #'linear)
    ((:quading-in :quad-in ) #'quad-in)
    ((:quading-in-out :quad-in-out ) #'quad-in-out)
    ((:quading-out :quad-out ) #'quad-out)
    ((:sine-in :sinusoidal-in ) #'sinusoidal-in)
    ((:sine-in-out  :sinusoidal-in-out) #'sinusoidal-in-out)
    ((:sine-out :sinusoidal-out ) #'sinusoidal-out)))

(defun animating (&key (by :none) the of to for at before)
  "A wrapper around ANIMATE that is aware of any default values defined in the
   the most recently enclosing form.

   BY is either an easing function, or a keyword that indicates an easing
   function. The valid keywords are :BOUNCE-OUT :BOUNCING-OUT :CUBING-IN
   :CUBICALLY-IN :CUBIC-IN :CUBING-IN-OUT :CUBICALLY-IN-OUT :CUBIC-IN-OUT
   :CUBING-OUT :CUBICALLY-OUT :CUBIC-OUT :ELASTIC-OUT :ELASTICALLY-OUT
   :LINEAR :LINEARLY :QUADING-IN :QUAD-IN :QUADRATICALLY-IN :QUADING-IN-OUT
   :QUAD-IN-OUT :QUADRATICALLY-IN-OUT :QUADING-OUT :QUAD-OUT :QUADRATICALLY-OUT
   :SINE-IN :SINUSOIDAL-IN :SINUSOIDALLY-IN :SINE-IN-OUT :SINUSOIDALLY-IN-OUT
   :SINUSOIDAL-IN-OUT :SINE-OUT :SINUSOIDAL-OUT :SINUSOIDALLY-OUT

   THE specifies the ACCESSOR to use for the tween. It should be as symbol
   naming a setf-able function.

   OF becomes the value of the TARGET slot of this TWEEN. If OF is NIL,
   then any default target value already set in an enclosing form is used.

   TO becomes the value of the END-VAL slot this TWEEN.

   FOR becomes the value of the DURATION slot of this TWEEN, superseding any
   default value set in the enclosing form.

   AT specifies a START-TIME, and overrides.
   "
  (animate (or of (when-bound *target*))
           the
           to
           :start (or at (when-bound *start-time*) 0)
           :on-complete before
           :ease (if (functionp by) by (keyword->ease-fn by))
           :duration (or for (when-bound *duration*) 0)))

