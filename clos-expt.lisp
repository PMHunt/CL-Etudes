(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "The foundation of all locks"))

(defclass null-lock (lock)
  ()
  (:documentation "A lock that is always free"))

(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "A lock that is either free or busy"))

(setq *null-lock*
      (make-instance 'null-lock :name "Null Lock"))

(setq *simple-lock*
      (make-instance 'simple-lock :name "Simple Lock"))

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))
