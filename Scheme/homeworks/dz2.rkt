(define (my-range a b d)
  (if (>= a b)
      (list)
      (cons a (my-range (+ a d) b d))))

(display "(my-range  0 11 3) -> ")
(display (my-range  0 11 3))
(newline)


(define (my-flatten xs)
  (if (null? xs)
      (list)
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))

(display "(my-flatten '((1) 2 (3 (4 5)) 6)) -> ")
(display (my-flatten '((1) 2 (3 (4 5)) 6)))
(newline)


(define (my-element? x xs)
  (if (null? xs)
      #f
      (if (equal? x (car xs))
          #t
          (my-element? x (cdr xs)))))

(display "(my-element? 1 '(3 2 1)) -> ")
(display (my-element? 1 '(3 2 1)))
(newline)
(display "(my-element? 4 '(3 2 1)) -> ")
(display (my-element? 4 '(3 2 1)))
(newline)


(define (my-filter pred? xs)
  (if (null? xs)
      (list)
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs)))))

(display "(my-filter odd? (my-range 0 10 1)) -> ")
(display (my-filter odd? (my-range 0 10 1)))
(newline)
(display "(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)) -> ")
(display (my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1)))
(newline)


(define (my-fold-left op xs)
  (if (< (length xs) 2)
      (car xs)
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))

(define (my-fold-right op xs)
  (if (< (length xs) 2)
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(display "(my-fold-left  quotient '(16 2 2 2 2)) -> ")
(display (my-fold-left  quotient '(16 2 2 2 2)))
(newline)
(display "my-fold-left  quotient '(1)) -> ")
(display (my-fold-left  quotient '(1)))
(newline)
(display "(my-fold-right expt     '(2 3 4)) -> ")
(display (my-fold-right expt     '(2 3 4)))
(newline)
(display "((my-fold-right expt     '(2)) -> ")
(display (my-fold-right expt     '(2)))
(newline)


(define (list->set xs)
  (if (< (length xs) 2)
      xs
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))

(display "((list->set '(1 1 2 3)) -> ")
(display (list->set '(1 1 2 3)))
(newline)


(define (set? xs)
  (if (< (length xs) 2)
      #t
      (if (my-element? (car xs) (cdr xs))
          #f
          (set? (cdr xs)))))

(display "(set? '(1 2 3)) -> ")
(display (set? '(1 2 3)))
(newline)
(display "(set? '(1 2 3 3)) -> ")
(display (set? '(1 2 3 3)))
(newline)
(display "(set? '())  -> ")
(display (set? '()) )
(newline)


(define (union xs ys)
  (list->set (append xs ys)))

(display "(union '(1 2 3) '(2 3 4))  -> ")
(display (union '(1 2 3) '(2 3 4)))
(newline)


(define (intersection xs ys)
  (if (null? xs)
      xs
      (if (my-element? (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

(display "(intersection '(1 2 3) '(2 3 4))  -> ")
(display (intersection '(1 2 3) '(2 3 4)))
(newline)


(define (difference xs ys)
  (if (null? xs)
      xs
      (if (my-element? (car xs) ys)
          (difference (cdr xs) ys)
          (cons (car xs) (difference (cdr xs) ys)))))

(display "(difference '(1 2 3 4 5) '(2 3))  -> ")
(display (difference '(1 2 3 4 5) '(2 3)))
(newline)


(define (symmetric-difference xs ys)
  (append (difference xs ys) (difference ys xs)))

(display "(symmetric-difference '(1 2 3 4) '(3 4 5 6))  -> ")
(display (symmetric-difference '(1 2 3 4) '(3 4 5 6)))
(newline)


(define (set-eq? xs ys)
  (define (set-equal? xr yr)
    (if (null? xr)
        #t
        (if (my-element? (car xr) yr)
            (set-equal? (cdr xr) yr)
            #f)))
  (if (= (length xs) (length ys))
      (set-equal? xs ys)
      #f))

(display "(set-eq? '(1 2 3) '(3 2 1))  -> ")
(display (set-eq? '(1 2 3) '(3 2 1)))
(newline)
(display "(set-eq? '(1 2) '(1 3))  -> ")
(display (set-eq? '(1 2) '(1 3)))
(newline)


(define (my-flatten-achive xs)
  (if (null? xs)
      (list)
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs))))))



                          