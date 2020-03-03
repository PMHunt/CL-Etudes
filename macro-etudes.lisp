;; Experiments while studying macros

;; with PCL, let's make a macro to create a 'do-like' fn against prime numbers

(defun primep (number)
  "Brute-force predicate for detecting primes.
Function isqrt returns greatest integer <= the exact positive square root of number
FOR loop iterates to isqrt and tries each fac as divisor to our number.
Condition NEVER terminates LOOP if a divisor is found,that is to say, if
(zerop (mod number fac)) returns T. If number is prime, the zerop test will stay nil
and the 'never' condition terminates the loop with T. This is our test for prime"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Keep iterating from our number to the next prime and return that prime"
  (loop for n from number when (primep n) return n))

;; We want to be able to write
;;
;; (do-primes (p 0 19)
;; "Exacute function body 'format blah' for each prime p between 0 and 19"
;;   (format t "~d " p))
;;
;; We could write something to do that using standard 'DO'
;;
(do ((p (next-prime 0) (next-prime (+ p 1))))
    ((> p 19))
  (format t "~d " p))

;; Which gives us a starting point for (defmacro do-primes ..

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (+ ,var 1))))
       ((> ,var ,end))
     ,@body))
