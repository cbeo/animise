(in-package :animise)

(defmacro when-bound (symbol)
  `(when (boundp (quote ,symbol)) ,symbol))

(defvar *duration*)
(defvar *target*)
(defvar *start-time*)

(defmacro sequencing ((&key loop-mode start starting with-target targeting target) &body forms)
  "START and STARTING are synonym keywords, and are used to specify as tart time.

   WITH-TARGET, TARGETING, and TARGET are synonym keywords, used to specify a
   default tween target for all tweens made within the body of this form.

   LOOP-MODE is supplied to the LOOP-MODE slot on the TWEEN-SEQ class that this
   form returns."
  (let ((this-seq (gensym))
        (dyn-vars (append (when (or with-target target targeting)
                             `((*target* ,(or with-target target targeting))))
                           (when (or start starting)
                             `((*start-time* ,(or start starting)))))))
    `(let ,dyn-vars
       (let ((,this-seq (in-sequence ,@forms)))
         (setf (loop-mode ,this-seq) ',loop-mode)
         ,this-seq))))


(defun pausing (&key for duration start)
  "A light wrapper around PAUSE. Is aware of default parameters that may have
   been set in an enclosing form."
  (pause (or for duration (when-bound *duration*) 0)
         (or start (when-bound *start-time*) 0)))

(defmacro grouping ((&key duration for with-duration) &body forms)
  "Returns a TWEEN-GROUP instance.

   DURATION, FOR, and WITH-DURATION are all synonym keywords, used to specify a default
   duration for any tweens define din this body, where applicable."
  (let ((dyn-vars (append (when (or duration for with-duration)
                            `((*duration* ,(or duration for with-duration)))))))
    `(let ,dyn-vars
       (as-group ,@forms))))


(defun keyword->ease-fn (name)
  (case name
    ((:bounce-out :bouncing-out) #'bounce-out)
    ((:cubing-in :cubically-in :cubic-in) #'cubic-in)
    ((:cubing-in-out :cubically-in-out :cubic-in-out) #'cubic-in-out)
    ((:cubing-out :cubically-out :cubic-out) #'cubic-out)
    ((:elastic-out :elastically-out) #'elastic-out)
    ((:linear :linearly) #'linear)
    ((:quading-in :quad-in :quadratically-in) #'quad-in)
    ((:quading-in-out :quad-in-out :quadratically-in-out) #'quad-in-out)
    ((:quading-out :quad-out :quadratically-out) #'quad-out)
    ((:sine-in :sinusoidal-in :sinusoidally-in) #'sinusoidal-in)
    ((:sine-in-out :sinusoidally-in-out :sinusoidal-in-out) #'sinusoidal-in-out)
    ((:sine-out :sinusoidal-out :sinusoidally-out) #'sinusoidal-out)))

(defun animating (ease &key the modifying of to for start starting)
  "A wrapper around ANIMATE that is aware of any default values defined in the
   the most recently enclosing form.

   EASE is either an easing function, or a keyword that indicates an easing
   function. The valid keywords are :BOUNCE-OUT :BOUNCING-OUT :CUBING-IN
   :CUBICALLY-IN :CUBIC-IN :CUBING-IN-OUT :CUBICALLY-IN-OUT :CUBIC-IN-OUT
   :CUBING-OUT :CUBICALLY-OUT :CUBIC-OUT :ELASTIC-OUT :ELASTICALLY-OUT
   :LINEAR :LINEARLY :QUADING-IN :QUAD-IN :QUADRATICALLY-IN :QUADING-IN-OUT
   :QUAD-IN-OUT :QUADRATICALLY-IN-OUT :QUADING-OUT :QUAD-OUT :QUADRATICALLY-OUT
   :SINE-IN :SINUSOIDAL-IN :SINUSOIDALLY-IN :SINE-IN-OUT :SINUSOIDALLY-IN-OUT
   :SINUSOIDAL-IN-OUT :SINE-OUT :SINUSOIDAL-OUT :SINUSOIDALLY-OUT

   THE and MODIFYING are synonym keywords. Either one specifies the ACCESSOR to
   use for the tween.

   OF becomes the value of the TARGET slot of this TWEEN. If OF is NIL,
   then any default target value already set in an enclosing form is used.

   TO becomes the value of the END-VAL slot this TWEEN.

   FOR becomes the value of the DURATION slot of this TWEEN, superseding any
   default value set in the enclosing form.

   START and STARTING are synonym keywords, specifiying a START-TIME.
   "
  (animate (or of (when-bound *target*))
           (or the modifying)
           to
           :start (or start starting (when-bound *start-time*) 0)
           :ease (if (functionp ease) ease (keyword->ease-fn ease))
           :duration (or for (when-bound *duration*) 0)))

