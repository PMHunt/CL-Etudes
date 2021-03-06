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

(defun two-to-four (lst)4
       (remove-if-not #'(lambda (x) (< 1 x 5)) lst))

(defun count-term (term lst)
  (length (remove-if-not #'(lambda (elt) (equal elt term)) lst)))

(defun list-len-two (lst)
  (remove-if-not #'(lambda (elt) (= (length elt) 2)) lst) )

(defun my-intersection (lst1 lst2)
  (remove-if-not #'(lambda (elt) (member elt lst1)) lst2))

(defun rank (card)
  (first card))

(defun suit (card)
  (first (last card)))

(defvar *my-hand* '((3 hearts)
                    (5 clubs)
                    (2 diamonds)
                    (4 diamonds)
                    (ace spades)))

(defun count-suit (k hand)
  (length (remove-if-not #'(lambda (e) (equal (suit e) k)) hand)))

(defvar *colors* '((clubs black)
                   (diamonds red)
                   (spades black)
                   (hearts red)))
(defun color-of (card)
  (first (last (assoc (suit card) *colors*))))

(defun first-red (hand)
  (car (remove-if-not #'(lambda (card) (equal 'red (color-of card)) ) hand)))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card) (equal 'black (color-of card)) ) hand))

(defun all-kind (k hand)
  (remove-if-not #'(lambda (card) (equal (suit card) k )) hand))

(defun what-ranks (k hand)
  (mapcar #'(lambda (card) (first card)) (all-kind k hand) ) )

(defvar *all-ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (x y)
  (member x (member y *all-ranks*)))

(defun rank-list (hand)
  (mapcar #'(lambda (e) (rank e)) hand))

(defun high-rank (hand)
  (find-if #'(lambda (rk) (member rk (rank-list hand))) (reverse *all-ranks*)) )

(defun high-card (hand)
  (assoc (high-rank hand) hand))

;; do high-card again with higher-rank-p and reduce

(defun length-of-lists (l)
  (length (reduce #'append l)))

(defun all-odd (list-of-numbers)
  "return t if all list-of-numbers members are odd numbers"
  (every #'oddp list-of-numbers))

(defun all-not-odd (list-of-numbers)
  "return t if none of list-of-numbers members are odd numbers"
  (every #'(lambda (n) (not (oddp n))) list-of-numbers))

(defun not-all-odd (list-of-numbers)
  (and (some #'oddp list-of-numbers)  (some #'evenp list-of-numbers) ) )

(defvar *blockdb* '((b1 shape brick)
                    (b1 color green)
                    (b1 size small)
                    (b1 supported-by b2)
                    (b1 supported-by b3)
                    (b2 shape brick)
                    (b2 color red)
                    (b2 size small)
                    (b2 supports b1)
                    (b2 left-of b3)
                    (b3 shape brick)
                    (b3 color red)
                    (b3 size small)
                    (b3 supports b1)
                    (b3 right-of b2)
                    *blackdb*
                    (b4 shape pyramid)
                    (b4 color blue)
                    (b4 size large)
                    (b4 supported-by b5)
                    (b5 shape cube)
                    (b5 color green)
                    (b5 size large)
                    (b5 supports b4)
                    (b6 shape brick)
                    (b6 color purple)
                    (b6 size large)))

(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(defun match-element (x y)
  (cond
    ((equal x y) t)
    ((equal y '?) t)
    (t nil)))

(defun match-triple (a p)
  (and (match-element (first a) (first p))
       (match-element (second a) (second p))
       (match-element (first (last a)) (first (last p)))))

(defun fetch (p)
  (remove-if-not #'(lambda (e) (match-triple e p)) *blockdb* ))

(defun color-pattern (x)
  (list  x 'color '?) )

(defun supporters (b)
  (mapcar #'caddr (fetch (list b 'supported-by '?))))

(defun is-cube-p (b)
  (equal 'cube (caddar (fetch (list b 'shape '?)))))

(defun supp-cube (b)
  (remove-if-not #'(lambda (a) (is-cube-p a)) (supporters b))) ; FIXME

(defun desc1 (b)
  (fetch (list b '? '?)))

(defun desc-list (b)
  (mapcar #'(lambda (e) (cdr e) ) (desc1 b) ))

(defun description (b)
  (apply #'append (desc-list b))) ; above external functions don't behave like lexical closures e.g #'append here

(defvar *words* '((one un)
                  (two deux)
                  (three trois)
                  (four quatre)
                  (five cinq)))

(defun add-lang (d l)
  (mapcar #'(lambda (e f) (append e (list f))) d l)) ; uses two parameter version of mapcar to add a new lang l/ to a lexicon d

(defun inalienable-rights (fn)
  (funcall fn '(life liberty and the pursuit of happiness))) ; funcall demo, contrast with apply

(defun today ()
  (let ((today-l (list (decode-universal-time (get-universal-time)))))
    (car today-l)))

(defun any-odd-p (x)
  (cond((null x) nil)
       ((oddp (first x)) t)
       (t (any-odd-p (rest x)))))

(defun if-any-odd-p (x)
  (if (null x)
      nil
      (if (oddp (first x)) t
          (if-any-odd-p (rest x)))))

(defun laugh (x)
  (cond ((equal 0 x) nil)
        (t  (cons 'ha (laugh (- x 1))))))

(defun addup (l)
  (cond ((null l) 0 )
        (t (+ (car l) (addup (cdr l))))))


(defun collatz (n)
  (cond ((equal n 1) t)
        ((evenp n) (c (/ n 2)))
        (t (c (+ (* 3 n) 1)))))

(defun fib (n)
  (cond
    ((equal n 1) 1)
    ((equal n 0) 0)
    (t (+ (fib (- n 1)) (fib (- n 2))))))

(defun range (max &key (min 0) (step 1))
  ;; jumped ahead and M-w C-y'd this one
  (loop for n from min below max by step
     collect n))

(defun fib-series (n)
  (mapcar #'fib (range n)))

(defun any-7-p (l)
  (cond
    ((equal 7 (first l)) t )
    ((equal nil (first l)) nil)
    (t (any-7-p (rest l)))))

(defun fact (n)
  ;; fact needs to unwind the stack from * n,
  (cond ((< n 0) nil)
        ((equal n 0) 1)
        (t (* n (fact (- n 1))))))

(defun find-first-odd (l)
  ;; this function doesn't seem to need to unwind the stack as tail call simple
  (cond
    ((null l) nil)
    ((oddp (first l)) (first l))
    (t (find-first-odd (rest l)))))

(defun last-element (l)
  (cond
    ((null (second l)) (first l))
    (t (last-element (rest l)))))

(defun add-nums (n)
  (if (= n 0)
      0
      (+ n (add-nums (- n 1)))))

(defun all-equal (l)
  ;; spec says all lists under 2 are T, so don't need nil test in original
  (cond
    ((< (length l) 2) t)
    (t (and (equal (first l) (second l)) (all-equal (rest l))))))


;; N - Cons a - Cons b
;; -------------------
;; 5 - 'Ha - (laugh 4)
;; 4 - 'Ha - (Laugh 3)
;; 3 - 'Ha - (laugh 2)
;; 2 - 'Ha - (laugh 1)
;; 1 - 'Ha - Nil

(defun countdown (n)
  (cond ((zerop n) nil)
        (t (cons n (countdown (- n 1))))))

(defun fact-app (n)
  (reduce #'* (countdown n)))

(defun countdown-0 (n)
  (defun countdown-inner (n)
    (cond ((zerop n) nil)
          (t (cons n (countdown-inner (- n 1))))))
  (append (countdown-inner n) '(0)))

(defun square-list (l)
  ;; LATER - can we generalise this? How does mapcar actually work?
  (cond ((null l) nil)
        (t (cons (* (car l) (car l)) (square-list (cdr l)) ))))

(defun my-nth (n l)
  (cond
    ((< n 0) nil)
    ((= n 0) (car l))
    (t (my-nth (- n 1) (cdr l)))))

(defun my-member (x l)
  (cond
    ((equal (car l) x) l)
    (t (my-member x (cdr l)))))

(defun my-assoc (x al)
  ;; pairlis was useful for making dotpairs
  (cond
    ((eql x (car (car al))) (car al))
    (t (my-assoc x (cdr al)))))

(defun compare-lengths (l m)
  (cond
    ((and (null (cdr l)) (null (cdr m))) 'same-length)
    ((null (cdr l)) 'second-is-longer)
    ((null (cdr m)) 'first-is-longer)
    (t (compare-lengths (cdr l) (cdr m)))))


(defun sum-numeric-elements (l)
  (defun find-numeric-elements (l)
    (cond
      ((null l) nil)
      ((numberp (car l)) (cons (car l) (find-numeric-elements (cdr l))))
      (t (find-numeric-elements (cdr l)))))
  (reduce #'+ (find-numeric-elements l)))

(defun my-remove (x l)
  (cond
    ((null l) nil)
    ((not (eql x (car l))) (cons (car l) (my-remove x (cdr l))))
    (t (my-remove x (cdr l)))))

(defvar list1 '(1 1 2 3 4 a b c))

(defvar list2 '(1 4 5 b c d))

(defun my-member (x l)
  (cond ((null l) nil)
        ((eql x (car l)) (cons (car l) (my-member x (cdr l))))
        (t (my-member x (cdr l)))))

(defun my-intersection (l m)
  (cond
    ((null l) nil)
    ((my-member (car l) m) (cons (car l) (my-intersection (cdr l) m)))
    (t (my-intersection (cdr l) m))))


(defun my-set-difference (l m)
  ;; book agrees but fails on 4 for (my-set-difference '(1 2 2 3) '(2 2 4))
  ;; FIXME
  (cond
    ((null l) nil)
    ((not (member (car l) m)) (cons (car l) (my-set-difference (cdr l) m)))
    (t (my-set-difference (cdr l) m))))

(defun list-odd-CR (l)
  (cond
    ((null l) nil)
    ((oddp (car l)) (cons (car l) (list-odd-CR (cdr l)) ))
    (t (list-odd-CR (cdr l)))))

(defun count-odd-CR (l)
  (length (list-odd-CR l)) )

(defun combine (x y)
  (+ x y))

(defun comb-fib (n)
  "Made this version to be able to dtrace the 'combine' function"
  (cond
    ((equal n 1) 1)
    ((equal n 0) 0)
    (t (combine (comb-fib (- n 1)) (comb-fib (- n 2))))))

(defun atoms-to-q (the-tree)
  (cond
    ((null the-tree) nil)
    ((atom the-tree) 'Q)
    (t (cons (atoms-to-q (first the-tree))
             (atoms-to-q (rest the-tree))))))


(defun count-atoms (the-tree)
  (if (atom the-tree)
      1
      (+ (count-atoms (first the-tree))
         (count-atoms (rest the-tree)))))

(defun count-cons (the-tree)
  (cond
    ((atom the-tree) 0)
    (t (+ 1
          (count-cons (first the-tree))
          (count-cons (rest the-tree))))))


(defun sum-tree (the-tree)
  (cond
    ((numberp the-tree) the-tree)
    ((atom the-tree) 0)
    (t (combine
        (sum-tree (first the-tree))
        (sum-tree (rest the-tree))))))


(defun my-subst (s-to s-from the-tree)
  (cond
    ((eql s-from the-tree) s-to)
    ((atom the-tree) the-tree)
    (t (cons (my-subst s-to s-from (first the-tree))
             (my-subst s-to s-from (rest the-tree))))))


(defun flatten-with-append (the-tree)
  "O(n2) conses for n cells, so this is kind of a shit way to do it"
  (cond
    ((null the-tree) nil)
    ((atom the-tree) (list the-tree))
    (t (append (flatten-with-append (first the-tree))
               (flatten-with-append (rest the-tree))))))

(defun flatten-with-acc (the-tree &optional acc)
  "Better way using an accumulator. PAIP p329"
  (cond
    ((null the-tree) acc)
    ((atom the-tree) (cons the-tree acc))
    (t (flatten-with-acc (first the-tree)
                         (flatten-with-acc (rest the-tree) acc)))))

(defun tree-depth (the-tree)
  "Return the maximum depth of a tree"
  (cond
    ((atom the-tree) 0)
    (t (+ 1 (max
             (tree-depth (first the-tree))
             (tree-depth (rest the-tree)))))))


(defun paren-depth (the-tree)
  "Return the maximum depth of parameters"
  (cond
    ((atom the-tree) 0)
    (t (max
        (1+ (paren-depth (first the-tree)))
        (paren-depth (rest the-tree))))))

(defun count-back-recur (n)
  "Count-up without a helper function"
  (if (< n 1)
      nil
      (append (count-back-recur (- n 1)) (list n))))

(defun make-loaf (n)
  "Make a list of X's"
  (if (< n 1)
      nil
      (append (make-loaf (- n 1)) (list 'X))))

(defun add-brackets (x n)
  "Add n brackets to x"
  (if (< n 1)
      x
      (list (add-brackets x (- n 1)))))

(defun pairings (l m)
  "Make pairs out of the elements two equal sized lists"
  (if
   (null (first l))
   nil
   (cons (list (first m) (first l))  (pairings (rest l) (rest m)) ) ))

(defun fact (n)
  "Demonstrate the Lisp debugger"
  (cond ((zerop n) (break "N is zero"))
        (t (* n (fact (- n 1))))))

;; 8.60 Keyboard exercise

(defvar *family*
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica))
  "name father mother")

(defun father (person)
  "person -> person
finds the father of a person if known, else nil"
  (second (assoc person *family*)))

(defun mother (person)
  "person -> person
finds the mother of a person if known, else nil"
  (third (assoc person *family*)))

(defun parents (person)
  "person -> (persons)
finds the mother and father of a person if known, else nil"
  (remove nil (rest (assoc person *family*))))

(defun children (person)
  "person -> (persons)
finds the children of a person if known, else nil"
  (if (null person)
      nil
      (mapcar #'first
              (remove-if-not #'(lambda (x) (or (equal person (second x)) (equal person (third x)))) *family*))))

(defun siblings (person)
  "person -> (persons)
finds the brothers and sisters of a person if known, else nil"
  (remove person
          (mapcar #'first
                  (remove-if-not #'(lambda (x) (intersection (parents person)(parents (first x)))) *family*))))

(defun mappend (fn the-list)
  "function, list -> list
Apply a fn to each item in the list then append resulting items"
  (apply #'append (mapcar fn the-list)))

(defun mapunion (f l)
  "function, list -> list
remove duplicates so it's a union of the sets"
  (remove-duplicates (mappend f l)))

(defun grandparents (person)
  "person -> (persons)
finds the grandparents of a person if known, else ni"
  (mapunion #'parents (parents person)))

(defun grandchildren (person)
  "person -> (persons
Needed for distant-cousins"
  (mapunion #'children (children person)))

(defun distant-cousins (person)
  "person -> (persons)
finds the cousins of a person if known, else nil.
Defined by having grandparents in common"
  (mappend #'grandchildren (grandparents person)))

(defun aunts-uncles (person)
  "person -> (person)
Person's parents siblings"
  (mapunion #'siblings (parents person)))

(defun first-cousins (person)
  "person -> (persons)
Children persons parents siblings"
  (mapunion #'children (aunts-uncles person)))

(defun descended-from-p (x y)
  "person person -> boolean
  returns true if first person descended from second
  (descended-from-p 'tamara 'arthur) -> true"
  (cond
    ((null x) nil)
    ((member y (parents x)) t)
    (t (or (descended-from-p (mother x) y) (descended-from-p (father x) y)))))

(defun ancestors (person)
  "person -> (persons)
  person's ancestors are their parents, parents parents, etc until tree runs out
  (ancestors 'marie) -> (ellen arthur kate george frank linda)  "
  (cond
    ((null (parents person)) nil)
    (t (append (parents person) (mapunion #'ancestors (parents person))))))

(defun generation-gap-helper (person predecessor n)
"person person number -> number
if we run out of ancestors, nil
if we match, n
recursivel see if person's mother or father is the predecessor"
  (cond
    ((null person) nil)
    ((equal person predecessor) n)
    (t (or
        (generation-gap-helper (father person) predecessor (+ 1 n))
        (generation-gap-helper (mother person) predecessor (+ 1 n))))))

(defun generation-gap (person predecessor)
  "person person -> number
count generations between person and ancestors
suzanne colin -> 1, fredrick colin -> 3, fredrick linda -> nil "
  (generation-gap-helper person predecessor 0))

;; FIXME -  fed up with recursion for now, so leaving advanced stuff to later

;;; New topic - IO

(defun print-old-bold ()
  (format t "~&There are old pilots")
  (format t "~&and there are bold pilots")
  (format t "~&but no old, bold pilots"))

(defun draw-line (n)
  " number -> nil
 side-effect is a line of n asterisks"
  (if (= n 0)
      (format t "~&")
      (progn
        (format t "*")
        (draw-line (- n 1)))))

(defun draw-box (x y)
  "Number number -> nil + side-effect
Draw a box of specified dimensions using *'s"
  (if (= y 0)
      (format t "~&")
  ""    (progn
        (draw-line x)
        (draw-box x (- y 1)))))

(defun 99-bottles (n)
  (if (= n 0)
      (format t "~& shit!")
      (progn
        (format t "~& ~S bottles of beer on the wall" n)
        (format t "~& ~S bottles of beer" n)
        (format t "~& take one down")
        (format t "~& pass it around")
        (format t "~& ~S bottles of beer on the wall ~%~%" (- n 1))
        (99-bottles (- n 1)))))

(defun print-board (board-list)
  "Take a list of X O and nil representing game state and print the board"
  (if (= (length board-list) 0)
      ; if all elements of the content list have been printed, return
      (format t "~&")
      ; using progn because we want side effects, printing to stdio
      (progn
        ; print the elements of the cell before we print cell contents
        (cond
          ((or (= (length board-list) 3)
               (= (length board-list) 6)) (format t "~& ------------- ~&"))
          ((not (or (= (length board-list) 0)
                    (= (length board-list) 9))) (format t " | ")))
        ; then print the contents of the cell
        (if (null (first board-list))
            (format t "   ")
            (format t " ~S " (first board-list)))
        ; recurse through  the contents list
        (print-board (rest board-list)))))

(defun print-pay ()
  "Compute gross pay based on user input of hourly rate and hours worked "
  (format t "Please supply hourly rate: ")
  (let ((rate (read)))
    (format t "Rate is ~S dollars/hour ~%" rate)
    (format t "Please supply hours worked: ")
    (let ((worked (read)))
      (format t "Worked ~S hours ~%" worked)
      (format t "Gross pay is ~S dollars" (* worked rate)))))

(defun cookie-monster ()
  "read user input until symbol COOKIE is recieved"
  (format t "Give me cookie ")
  (let ((user-input (read)))
    (format t "Cookie? ~S ~%" user-input)
    (if (equal user-input 'cookie)
        (format t "Thank you, munch, munch")
        (progn
          (format t "Ugh, no want ~S ~%" user-input)
          (cookie-monster)))))

(defun space-over (n)
  (if (= n 0)
      (format t "")
      (progn
        (format t " ")
        (space-over (- n 1)))))

(defun test-so (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun plot-one-point (plotting-string y-value)
  (space-over y-value)
  (format t "~S~%" plotting-string))

(defun plot-points (plotting-string point-list)
  (if (not (null point-list))
      (progn
        (plot-one-point plotting-string (first point-list))
        (plot-points plotting-string (rest point-list)))))

(defun generate (m n)
  (if (< m n)
      (if (equal m n)
          (list m)
          (cons m (generate (+ m 1) n)))
      ; so it doesn't break when < n m
      (if (equal m n)
          (list n)
          (cons n (generate (+ n 1) m)))))

(defun make-graph (func start end plotting-string)
(plot-points plotting-string (mapcar func (generate start end))))

(defparameter gleet-club '((John Smith) (Barbara Ann) (Muhammed Ali)))

(defun print-one-name (name)
    (format t "~&~10S ~S"
            (second name)
            (first name)))

(defun print-all-names (name-list)
    (mapcar #'print-one-name name-list)
    'done)



(setf *friends* nil)
(setf *popularity* 0)

(defun meet (person)
    (cond ((equal person (first *friends*))
           'we-just-met)
          ((member person *friends*)
           'we-know-each-other)
          (t (push person *friends*)
             (incf *popularity* 1)
             (format t "popularity ~S ~%" *popularity*)
             (list 'pleased-to-meet-you person))))

(defun forget (person)
    (cond ((not (member person *friends*))
           (format t "Not a member"))
          (t (remove person *friends*))))

;; 10.5

(defun less-ugly (x y)
  (let* ((max (if (> x y) x y))
        (avg (/ (+ x y) 2.0))
        (pct (* 100 (/ avg max))))
    (format t "Average ~a is ~a percent of ~a " avg pct max)))

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 ((< commission 100) 'poor))))
    (break "Value of RESULT is ~S and COMMISSION ~S" result commission)
    (format t "~&I predict you will be: ~S" result)
result))

;; 10.8 - see oxo.lisp
