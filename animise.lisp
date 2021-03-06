;;;; animise.lisp

(in-package #:animise)

;;; Interface generices


;; In addition to the following, both a `DURATION' method and
;; a `START-TIME' setf-able method are implemented for each class. 

(defgeneric run-tween (tween time))

;;; TWEENS

;; A TWEEN is effectively a function. A tween is used to produce a sequence of
;; values over time the purpose of animating an object.

;; START-TIME and DURATION arguments are a representation of time and must use
;; the same units. START-TIME must be supplied on TWEEN instantiation, and is
;; used to record when the tween begins running.

;; DELTA-VAL is a number, the amount by which the animated object's target
;; property should have changed by the time the animation is over.

;; Different EASE functions may be supplied to modulate the way that sequence is
;; produced.  EASE is the heart of the TWEEN's behavior.

;; Every TWEEN instance must have an EFFECTOR, which should be a closure that
;; accepts a single argument. The purpose of the EFFECTOR is to apply the values
;; generated by the TWEEN to some object.

(defclass tween ()
  ((start-time
    :accessor start-time
    :initarg :start-time
    :initform 0)
   (duration
    :reader get-duration
    :initarg :duration
    :initform 1000.0) ; 1 second
   (ease-fn
    :initarg :ease-fn
    :initform #'linear)
   (start-val
    :initform nil)
   (end-val
    :accessor end-val
    :initarg :end-val
    :initform (error "Must supply an end value."))
   (target
    :initarg :target
    :initform (error "Must have a target"))
   (rounding
    :initarg :rounding
    :initform t )
   (setter)
   (accessor
    :initarg :accessor
    :initform (error "Must supply an accessor function"))
   (on-complete
    :accessor on-complete
    :initarg :on-complete
    :initform nil)))

(defmethod initialize-instance :after ((tween tween) &key)
  (with-slots (getter setter accessor) tween
    (setf setter (eval `(function (setf ,accessor))))))


;;; TWEEN-SEQ combines tweens to run one after another.

(defclass tween-seq ()
  ((tweens
    :accessor tweens
    :initarg :tweens
    :initform (error "empty tween sequences are disallowed."))
   (loop-mode
    :accessor loop-mode
    :initarg :loop-mode
    :initform nil)))  ; :looping :reflecting (:looping max n) (:reflecting max n)

;;; TWEEN-GROUP

(defclass tween-group ()
  ((members
    :accessor members
    :initarg :members
    :initform nil
    :type list)))

;;; Some functions that use the protocol defined by the generics

(defun pause (duration &optional (start 0))
  (make-instance 'tween :target (list 0) :start-time start :duration duration
                 :accessor 'car :end-val duration :rounding nil))

(defun animate (target acc end &key
                                 (start 0)
                                 (ease #'linear)
                                 (rounding t)
                                 on-complete
                                 (duration 1000) )
  (make-instance 'tween
                 :target target
                 :start-time start
                 :accessor acc
                 :end-val end
                 :ease-fn ease
                 :rounding rounding
                 :on-complete on-complete
                 :duration duration))

(defun in-sequence (t1 &rest tws)
  "Run the provided tweens one after the other"
  (let ((seq (make-instance 'tween-seq :tweens (cons t1 tws))))
    (correct-sequencing seq)
    seq))

(defun end-time (tween)
  "Some tweens dont have a duration ,and hence never end. NIL is returned to
  reflect this."
  (when-let (dur (duration tween))
            (+ (start-time tween) dur)))

(defun tween-finished-p (tween time)
  "Returns T if  TWEEN is done running."
  (when-let (end (end-time tween))
            (>= time end)))

(defun add-to-group (group tween &key (offset 0))
  "Adds TWEEN to GROUP. If TWEEN is the first tween added to GROUP, then TWEEN'S
   start time becomes the GROUP'S start time and the OFFSET is ignored.
   Otherwise, TWEEN's start time is set to the start time of the GROUP modified
   by OFFSET."
  (when-let (start-time (and (members group) (start-time group)))
    (setf (start-time tween)
          (+ offset start-time)))
  (push (members group) tween))


(defun as-group (tween &rest tweens)
  "run the provided tweens in paralell (not actually, but
   logically). That is, tweens in this group can be updated at the same
   time."
  (make-instance 'tween-group :members (cons tween tweens)))

;;; Interface implementations for TWEEN class

(defmethod duration ((tween tween))
  (get-duration tween))

(defmethod run-tween ((tween tween) time)
  (with-slots (start-time duration rounding ease-fn start-val target end-val setter accessor) tween
    (when (>= time start-time)
      (when (null start-val)
        (setf start-val (funcall accessor target)))
      (let ((new-val
              (+ start-val
                 (funcall ease-fn
                          start-time
                          duration
                          time
                          (- end-val start-val)))))
        (funcall setter
                 (if rounding (round new-val) new-val)
                 target)))))

(defmethod run-tween :after ((tween tween) time)
  (when (and (on-complete tween) (tween-finished-p tween time))
    (funcall (on-complete tween))
    (setf (on-complete tween) nil)))

;;; Interface implementations for TWEEN-SEQ

(defmethod start-time ((ob tween-seq))
  (when (tweens ob)
    (start-time (car (tweens ob)))))

(defun correct-sequencing (seq)
  ;; A helper function that sets the start and stop time for each tween in a
  ;; tween sequence. It assumes the first tween is correctly configured.
  (when (tweens seq)
    (let ((end (end-time (car (tweens seq)))))
      (dolist (tween (cdr (tweens seq)))
        (setf (start-time tween) end)
        (setf end (end-time tween))))))


(defmethod (setf start-time) (val (ob tween-seq))
  (unless (tweens ob) (error "Cannot set start time of empty sequence."))
  (setf (start-time (car (tweens ob))) val)
  (correct-sequencing ob)
  val)


(defmethod duration ((ob tween-seq))
  "NIL means that the tween is infinitely looping."
  (unless (tweens ob) (error "Cannot determine the duration of an empty sequence."))
  (with-slots (tweens loop-mode) ob
    (match loop-mode
      (:looping nil)
      (:reflecting nil)
      ((list _ max _)
       (* max
          (reduce #'+ tweens :key #'duration :initial-value 0)))
      (nil
       (reduce #'+ tweens :key #'duration :initial-value 0)))))

(defun reset-child-loops (seq &optional reset-parent)
  ;; Resets LOOP-MODEs that have counts to whatever their initial count was.
  ;; Does the same for all tweens in the sequence that might themselves be
  ;; TWEEN-SEQ instances
  (dolist (sub (tweens seq))
    (when (typep sub 'tween-seq)
      (reset-child-loops seq)))
  (when reset-parent
    (with-slots (loop-mode) seq
      (when (consp loop-mode)
        (setf (nth 2 loop-mode)
              (nth 1 loop-mode))))))

;; TODO implmeent the reflecting tween behavior
(defun apply-looping (seq now)
  ;; Applies any looping in order to see if, after doing so, a runnable tween
  ;; becomes available.
  (with-slots (loop-mode) seq
    (match loop-mode

      (:looping
       ;; If you're simply looping, you need to make sure each non-infinite subloop
       ;; be reset.
       (reset-child-loops seq t)
       ;; then set the start time to right now and correct start times of the
       ;; subsequent loops
       (setf (start-time seq) now)
       (car (tweens seq)))

      ((list :looping _ n)
       (when (plusp n)
         (decf (nth 2 loop-mode))
         (reset-child-loops seq)
         (setf (start-time seq) now)
         (car (tweens seq)))) )))

      ;; (:reflecting
      ;;  (reverse-tween seq)
      ;;  (car (tweens seq)))

      ;; ((list :reflecting max n)
      ;;  (when (plusp n)
      ;;    (decf (nth 2 loop-mode))
      ;;    (reverse-tween seq)
      ;;    (car (tweens seq)))))))

(defun has-on-complete (thing)
  (and (typep thing 'tween)
       (on-complete thing)))

(defmethod run-tween ((ob tween-seq) time)
  (when-let (tween (or
                    ;; find the first unfinished tween in the sequence
                    (find-if-not (lambda (tween)
                                   (when (tween-finished-p tween time)
                                     (when (has-on-complete tween)
                                       (funcall (on-complete tween))
                                       (setf (on-complete tween) nil))
                                     t))
                                 (tweens ob))
                    ;; otherwise apply any looping configuration on the sequence
                    ;; and apply the tween that results
                    (apply-looping ob time)))
            (run-tween tween time)))


;;; Interface Implementations for TWEEN-GROUP

(defmethod start-time ((ob tween-group))
  (loop :for tw :in (members ob) :minimizing (start-time tw)))

(defmethod (setf start-time) (val (ob tween-group))
  (Unless (members ob) (error "Can't setf the start time on an empty group"))
  (let* ((old-start-time (start-time ob))
         (offset (- val old-start-time)))
    (dolist (tween (members ob))
      (incf (start-time tween) offset))
    val))

(defmethod duration ((ob tween-group))
  (when (members ob)
    (let ((start-time (start-time ob))
          (end-time (loop :for tw :in (members ob) :maximizing (end-time tw))))
      (- end-time start-time))))

(defmethod run-tween ((ob tween-group) now)
  (dolist (tween (members ob))
    (run-tween tween now)))


