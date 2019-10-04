(in-package :animise)

(defmacro when-bound (symbol)
  `(when (boundp (quote ,symbol)) ,symbol))

(defvar *duration*)
(defvar *target*)
(defvar *start-time*)

(defmacro sequencing ((&key loop-mode start starting with-target targeting target) &body forms)
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
  (pause (or for duration (when-bound *duration*) 0)
         (or start (when-bound *start-time*) 0)))

(defmacro grouping ((&key duration for with-duration) &body forms)
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

(defun animating (ease &key the  modifying of to for start starting)
  (animate (or of (when-bound *target*))
           (or the modifying)
           to
           :start (or start starting (when-bound *start-time*) 0)
           :ease (if (functionp ease) ease (keyword->ease-fn ease))
           :duration (or for (when-bound *duration*) 0)))

