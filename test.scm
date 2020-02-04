(define (find-smallest a b c)
  (cond
   ((and (<= a b) (<= a c)) a)
   ((and (<= b a) (<= b c)) b)
   ((and (<= c a) (<= c a)) c)))

(define (sum-i n)
  (define (iter n sofar)
    (if (= n 0 sofar)
        (+ (iter (- n 1) (+ n sofar)))))
    (iter n 0)
    )

(define (sum-of-squares x y)
  (+ (square x) (square y)))
