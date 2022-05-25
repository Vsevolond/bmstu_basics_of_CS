(define (count x xs)
  (if (null? xs)
      0
      (if (equal? (car xs) x)
          (+ (count x (cdr xs)) 1)
          (count x (cdr xs)))))

(display "(count 'a '(a b c a)) ⇒ ")
(count 'a '(a b c a))
(display "(count 'b '(a c d))   ⇒ ")
(count 'b '(a c d))
(display "(count 'a '())        ⇒ ")
(count 'a '())


(define (delete pred? xs)
  (if (null? xs)
      (list)
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))

(display "(delete even? '(0 1 2 3)) ⇒ ")
(delete even? '(0 1 2 3))
(display "(delete even? '(0 2 4 6)) ⇒ ")
(delete even? '(0 2 4 6))
(display "(delete even? '(1 3 5 7)) ⇒ ")
(delete even? '(1 3 5 7))
(display "(delete even? '()) ⇒ ")
(delete even? '())


(define (iterate f x n)
  (if (= n 0)
      (list)
      (cons x (iterate f (f x) (- n 1)))))

(display "(iterate (lambda (x) (* 2 x)) 1 6) ⇒ ")
(iterate (lambda (x) (* 2 x)) 1 6)
(display "(iterate (lambda (x) (* 2 x)) 1 1) ⇒ ")
(iterate (lambda (x) (* 2 x)) 1 1)
(display "(iterate (lambda (x) (* 2 x)) 1 0) ⇒ ")
(iterate (lambda (x) (* 2 x)) 1 0)


(define (intersperse e xs)
  (if (< (length xs) 2)
      xs
      (cons (car xs) (cons e (intersperse e (cdr xs))))))

(display "(intersperse 'x '(1 2 3 4)) ⇒ ")
(intersperse 'x '(1 2 3 4))
(display "(intersperse 'x '(1 2))     ⇒ ")
(intersperse 'x '(1 2))
(display "(intersperse 'x '(1))       ⇒ ")
(intersperse 'x '(1))
(display "(intersperse 'x '())        ⇒ ")
(intersperse 'x '())


(define (any? pred? xs)
  (and (not (null? xs)) (or (pred? (car xs)) (any? pred? (cdr xs)))))

(display "(any? odd? '(1 3 5 7)) ⇒ ")
(any? odd? '(1 3 5 7))
(display "(any? odd? '(0 1 2 3)) ⇒ ")
(any? odd? '(0 1 2 3))
(display "(any? odd? '(0 2 4 6)) ⇒ ")
(any? odd? '(0 2 4 6))
(display "(any? odd? '()) ⇒ ")
(any? odd? '())


(define (all? pred? xs)
  (or (null? xs) (and (pred? (car xs)) (all? pred? (cdr xs)))))

(display "(all? odd? '(1 3 5 7)) ⇒ ")
(all? odd? '(1 3 5 7))
(display "(all? odd? '(0 1 2 3)) ⇒ ")
(all? odd? '(0 1 2 3))
(display "(all? odd? '(0 2 4 6)) ⇒ ")
(all? odd? '(0 2 4 6))
(display "(all? odd? '()) ⇒ ")
(all? odd? '()) 


(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)))))

(display "(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))")
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))
(newline)
(display "((o f g h) 1) ⇒ ")
((o f g h) 1)
(display "((o f g) 1)   ⇒ ")
((o f g) 1)
(display "((o h) 1)     ⇒ ")
((o h) 1)
(display "((o) 1)       ⇒ ")
((o) 1)     


