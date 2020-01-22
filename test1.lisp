(load "sdraw.generic")

(setf  *line* '(roses are red))

(defun last-element (my-list)
  (first (reverse my-list)))

(defun my-butlast (my-list)
  (reverse (rest (reverse my-list))))

(defun contains-article-p (sent)
  (intersection '(a the an) sent))

(defun add-vowels (sent)
  (union '(a e i o u) sent))

(setf mvlist '(small red metal cube -vs- red plastic small cube) )

(defun right-side (sent)
  (rest (member '-vs- sent)))


(defun left-side (sent)
  (reverse  (rest (member '-vs- (reverse sent)))))

(defun count-common (sent)
  (length (intersection (right-side sent) (left-side sent))))

(defun compare (sent)
  (list (count-common sent) (intersection (right-side sent) (left-side sent))))

(defun who-wrote (sent)
  (first  (rassoc sent *books*)))

(defun wrote-what (sent)
  (rest (assoc sent *books*)))

(defparameter *nerdstates* '(sleeping eating waiting-for-a-computer programming debugging))

(defun nerdus (state)
  (if (eql state 'debugging) 'sleeping 
      (first (rest (member state *nerdstates*)))))


(defun sleepless-nerdus (state)
  (if (eql state 'debugging) 'eating 
      (first (rest (member state *nerdstates*)))))

(defun swap-first-with-last (sent)
  (let ((frontend (list (first sent))) (backend (last sent)))
    (append backend (reverse (rest (reverse (rest sent)))) frontend)))

(defun rotate-left (sent)
  (append (rest sent) (list (first sent))))

(defun rotate-right (sent)
  (append (last sent) (reverse (rest (reverse sent)))))

(defvar rooms

  '((living-room        (north front-stairs)
     (south dining-room)
     (east kitchen))

    (upstairs-bedroom   (west library) 
     (south front-stairs))

    (dining-room        (north living-room) 
     (east pantry)
     (west downstairs-bedroom))

    (kitchen            (west living-room)
     (south pantry))

    (pantry             (north kitchen)
     (west dining-room))

    (downstairs-bedroom (north back-stairs)
     (east dining-room))

    (back-stairs        (south downstairs-bedroom)
     (north library))

    (front-stairs       (north upstairs-bedroom)
     (south living-room))

    (library            (east upstairs-bedroom)
     (south back-stairs))))

(defun choices (sent)
  (rest (assoc sent rooms)))

(defun look (direction room)
  (last-element (assoc direction (choices room))))

(defparameter loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting loc"
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(Defun upstairsp (place)
  (if (member place '(library upstairs-bedroom)) t nil) )

(defun onstairsp (place)
  (if (member place '(front-stairs back-stairs)) t nil) )

(defun where ()
  (cond
    ((upstairsp loc) (list 'robbie 'is 'upstairs 'in 'the loc))
    ((onstairsp loc) (list 'robbie 'is 'on 'the 'stairs))
    (t (list 'robbie 'is 'downstairs 'in 'the loc))))

(defun move (direction)
  (if (not  (last-element (assoc direction (choices loc))))
      (list 'ow 'robbie 'hit 'a 'wall)
      (set-robbie-location  (last-element (assoc direction (choices loc))))))

(defun add1 (x)
  (+ x 1))

(defvar *daily-planet* '((olsen jimmy 123-76-4535 cub-reporter)
                         (kent clark 089-52-6787 reporter)
                         (land lois 951-26-1438 reporter)
                         (white perry 355-16-7439 editor)))

(defun get-ssn (table)
  (mapcar #'third table))

(defun find-zeros (lst)
  (mapcar #'zerop lst))

(defun greater-than-five (x    )
  (if (> x 5) t nil))

(defun find-greater-than-five (x)
  (mapcar #'greater-than-five x          ))

(defun subtract-7 (x)
  (mapcar #'( lambda (n) (- n 7)) x) )

(defun find-bool (x)
  (mapcar #'(lambda (n) (if (or  (equal n nil) (equal n t)) t nil)) x ))

(defun flip-orientation (x)
  (mapcar #'(lambda (e) (if (equal e 'up) 'down 'up)) x ))

(defun roughly-equal (x k)
  (find-if #'(lambda (entry) (and (< entry (+ 10 k)) (> entry (- k 10)) ) ) x ) )

(defun find-nested (x)
  (find-if #'(lambda (entry) (not (equal entry nil))) x ))

(defvar *note-table* '((c 1)
                       (c-sharp 2)
                       (d 3)
                       (d-sharp 4)
                       (e 5)
                       (f 6)
                       (f-sharp 7)
                       (g 8)
                       (g-sharp 9)
                       (a 10)
                       (a-sharp 11)
                       (b 12)))

(defun numbers (note-l)
  (mapcar #'(lambda (entry) (first  (last (assoc entry *note-table*)))) note-l ))

(defun dotlist (lst)
  (mapcar #'(lambda (element) (cons (car element) (car (cdr element)))) lst)) ; converts table to dot.pair to enable rassoc

(defun notes (num-l)
  (mapcar #'(lambda (k) (first (rassoc k (dotlist *note-table*)))) num-l))

(defun raise (n num-l)
  (mapcar #'(lambda (element)
              (+ n element)) num-l))

(defun normalize (num-l)
  (mapcar #'(lambda (element) (cond
                           ((> element 12) (- element 12))
                           ((< element 1) (+ element 12))
                           (t element)))
          num-l))

(defun transpose (n note-l)
  (notes  (normalize (raise n (numbers note-l)))  ))
