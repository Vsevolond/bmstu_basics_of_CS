(define (day-of-week day month year)
  (define a (quotient (- 14 month) 12))
  (let ( (y (- year a))
         (m (- (+ (* 12 a) month) 2)))
  (remainder (- (+ day (quotient (* 31 m) 12) y (quotient y 4) (quotient y 400)) (quotient y 100)) 7)))

(display "(day-of-week 04 12 1975) ⇒ ")
(day-of-week 04 12 1975)
(display "(day-of-week 04 12 2006) ⇒ ")
(day-of-week 04 12 2006)
(display "(day-of-week 29 05 2013) ⇒ ")
(day-of-week 29 05 2013)


(define f (lambda (a b c)
  (define d (- (* b b) (* 4 a c)))
  (cond ((< d 0) (list ))
        ((= d 0) (list (/ (- b) (* 2 a))))
        ((> d 0) (list (/ (+ (- b) (sqrt d)) (* 2 a)) (/ (- (- b) (sqrt d)) (* 2 a)))))))

(display "(f 1 2 1)        → ")
(f 1 2 1)

(define my-gcd (lambda (a b)
                 (if (= b 0)
                     a
                     (my-gcd b (remainder a b)))))

(define my-lcm (lambda (a b)
                 (/ (* a b) (my-gcd a b))))

(define prime? (lambda (x)
                 (let loop ((d 2))
                   (if (> (* d d) (abs x))
                       #t
                       (if (= (remainder (abs x) d) 0)
                           #f
                           (loop (+ d 1)))))))

(display "(my-gcd 3542 2464) ⇒ ")
(my-gcd 3542 2464)
(display "(my-lcm 3 4)       ⇒  ")
(my-lcm 3 4)
(display "(prime? 11)        ⇒  ")
(prime? 11)
(display "(prime? 12)        ⇒  ")
(prime? 12)       


