;;;; animise.lisp

(in-package #:animise)

(defun time-frac (start duration current)
  (let* ((end (+ start duration))
        (progress (max 0 (- end current))))
    (- 1.0 (/ progress duration))))

(defmacro def-ease (name &rest body)
  `(defun ,name (start duration current &optional (delta 1))
     (let ((frac (time-frac start duration current)))
       ,@body)))

(defmacro def-mirror-for (name-of-ease)
  (let ((mirror-name (read-from-string (format nil "mirror-~a" name-of-ease))))
    `(def-ease ,mirror-name
         (if (<= frac 0.5)
             (,name-of-ease start (* 0.5 duration) current delta)
             (,name-of-ease start (* 0.5 duration)
                            (- (+ start (* 0.5 duration))
                               (- current (+ start (* 0.5 duration))))
                            delta)))))

(def-ease linear (* delta frac))

(def-mirror-for linear)

(def-ease quad-in (* frac frac delta))

(def-mirror-for quad-in)

(def-ease quad-out (* frac (- frac 2.0) -1 delta))

(def-mirror-for quad-out)

(def-ease quad-in-out
  (setf frac (/ frac 0.5))
  (if (< frac 1) (* frac frac 0.5 delta)
      (progn (decf frac)
             (* -1 delta 0.5 (1- (* frac (- frac 2)))))))

(def-mirror-for quad-in-out)

(def-ease cubic-in (* frac frac frac delta))

(def-mirror-for cubic-in)

(def-ease cubic-out
    (decf frac)
    (* (1+ (* frac frac frac)) delta))

(def-mirror-for cubic-out)

(def-ease cubic-in-out
  (setf frac (/ frac 0.5))
  (if (< frac 1) (* delta 0.5 frac frac frac)
      (progn
        (decf frac 2)
        (* delta 0.5 (+ 2 (* frac frac frac))))))

(def-mirror-for cubic-in-out)

(def-ease sinusoidal-in
  (+ delta (* -1 delta (cos (* frac pi 0.5)))))

(def-mirror-for sinusoidal-in)

(def-ease sinusoidal-out
  (* delta (sin (* frac pi 0.5))))
(def-mirror-for sinusoidal-out)

(def-ease sinusoidal-in-out
  (* delta -0.5 (1- (cos (* pi frac)))))
(def-mirror-for sinusoidal-in-out)

(def-ease elastic-out
      (let ((sqrd (* frac frac))
            (cubed (* frac frac frac)))
        (* 100 delta (+ (* 0.33 sqrd cubed)
                        (* -1.06 sqrd sqrd)
                        (* 1.26 cubed)
                        (* -0.67 sqrd)
                        (* 0.15 frac)))))

(def-mirror-for elastic-out)


(def-ease bounce-out
    (let ((coeff 7.5627)
          (step (/ 1 2.75)))
      (cond ((< frac step)
             (* delta coeff frac frac))
            ((< frac (* 2 step))
             (decf frac (* 1.5 step))
             (* delta
                (+ 0.75
                   (* coeff frac frac))))
            ((< frac ( * 2.5 step))
             (decf frac (* 2.25 step))
             (* delta
                (+ 0.9375
                   (* coeff frac frac))))
            (t
             (decf frac (* 2.65 step))
             (* delta
                (+ 0.984375
                   (* coeff frac frac)))))))

(def-mirror-for bounce-out)

(defun frames (ease-fn &optional (step 0.1))
  (loop :for time :from 0 :upto (+ 1 step) :by step
        :collect (funcall ease-fn 0 1.0 time)))

(defun print-frames (fn &key (width 20) (mark #\.) (step 0.1))
  (loop for frame in (frames fn step) do
    (dotimes (x width) (princ #\Space))
    (dotimes (x (round (* frame width)))
      (princ #\Space))
    (princ mark)
    (terpri)))


