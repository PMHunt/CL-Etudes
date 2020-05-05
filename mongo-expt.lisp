(ql:quickload '(cl-mongo hunchentoot cl-who))

(defpackage :mongo-expt
  (:use :cl :hunchentoot :cl-who :cl-mongo))

(db.insert "foo" (kv "document" "one"))

(pp (iter (db.find "foo" 'all)))
;
(defvar *DOC* (make-document))
;
(add-element "tag" "key" *DOC*)

(add-element "array" (list 1 2 3 "hello") *DOC*)
;
(db.insert "foo" *DOC*)
;
(pp (iter (db.find "foo" 'all)))
