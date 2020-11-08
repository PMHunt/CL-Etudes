;; 16.3 Debugging

(defparameter *quux* 42)

(defun foo (x)
  (let ((*quux* 23))
    (bar (1- x) *quux*)))

(defun bar (a b)
  (declare (optimize debug))
  (let ((c (* b b)))
    (catch 'tag
      (baz c a))))

(defun baz (v w)
  (/ v w))
