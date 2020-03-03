(ql:quickload '(:dexador :plump :lquery :lparallel))

;; libraries required for lisp cookbook recepies

(defun sleep-random ()
  "Sleep 1-2 seconds"
  (sleep (+ 1 (/ (random 1000) 1000))))


(defmacro with-url ((doc url) &body body)
  "Execute `body' with `url' as a plump document bound to `doc'"
  `(let ((,doc (plump:parse (dex:get ,url))))
     ,@body))

;; Example
(with-url (doc "https://www.google.com")
  (format t "#title text: ~A" (plump:text (clss:select "#title" doc))))

;; based on commonlisp cookbook site

(plump:parse "<foo><bar this is=\"a thing\">baz</bar><span id=\"test\">oh my")

(defvar *url* "https://lispcookbook.github.io/cl-cookbook/")

(defvar *request* (dex:get *url*))

(defvar *parsed-content* (lquery:$ (initialize *request*)))
;; => #<PLUMP-DOM:ROOT {1009EE5FE3}>

(lquery:$ *parsed-content* "#content li")
;; inspector to find tag we need, li, #id is 'content'

(lquery:$  *parsed-content* "#content li" (serialize))
;; to see html in *parsed-content* we do 'serialize

(lquery:$  *parsed-content* "#content" (text))
;; or like this to get the text

(lquery:$  *parsed-content* "#content li a" (attr :href))
;; extract hrefs

(defvar *urls* (lquery:$  *parsed-content* "#content li a" (attr :href)))
;; are the URLs reachable?

(remove-if (lambda (it) (string= it "mailto:" :start1 0 :end1 (length "mailto:"))) *urls*)
;; filter out mailto
