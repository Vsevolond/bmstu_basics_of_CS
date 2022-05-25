(load "unit-test.scm")

;task 1

#|(define memoized-factorial
  (let loop ((known-results '()))
    (lambda (n)
      (if (<= n 1)
          1
          (let ((res (assoc n known-results)))
            (if res
                (cadr res)
                (let ((res (* n (memoized-factorial (- n 1)))))
                  (set! known-results (cons (list n res) known-results))
                  res)))))))|#

(define memoized-factorial
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg n)
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (if (<= n 1)
                           1
                           (* n (memoized-factorial (- n 1))))))
              (set! known-results (cons (list arg res) known-results))
              res))))))



;task 2

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (delay b)))))

(define (lazy-car p)
  (car p))

(define (lazy-cdr p)
  (force (cdr p)))

(define (lazy-head xs k)
  (if (= k 0)
      (list)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs n)
  (if (= n 0)
    (lazy-car xs)
    (lazy-ref (lazy-cdr xs) (- n 1))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))


(define (factorials)
  (let loop ((prime 1) (num 1))
    (lazy-cons (* prime num) (loop (* prime num) (+ num 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n)
            (- n 1)))



;task 3

(define (read-words)
  (let loop ((words '()) (str "") (c (read-char)))
    (if (eof-object? c)
        (if (> (string-length str) 0) (reverse (cons str words)) (reverse words))
        (if (or (equal? c #\.) (char-whitespace? c))
            (if (> (string-length str) 0)
                  (loop (cons str words) "" (read-char))
                  (loop words "" (read-char)))
              (loop words (string-append str (string c)) (read-char))))))



;task 4

(define ie (interaction-environment))

(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (field1 ...))
     (begin
       (eval (list 'define
                      (string->symbol (string-append "make-" (symbol->string 'name)))
                      (lambda (field1 ...)
                        (list (list 'type 'name) (list 'field1 field1) ...))) ie)
       (eval (list 'define
                      (string->symbol (string-append (symbol->string 'name) "?"))
                      (lambda (x)
                        (and (list? x) (not (null? x)) (list? (car x))
                             (let ((ares (assoc 'type x)))
                               (and ares (equal? (cadr ares) 'name)))))) ie)
       (eval (list 'define
                       (string->symbol (string-append (symbol->string 'name) "-" (symbol->string 'field1)))
                       (lambda (x)
                         (cadr (assoc 'field1 (cdr x))))) ie) ...
       (eval (list 'define
                       (string->symbol (string-append "set-" (symbol->string 'name) "-" (symbol->string 'field1) "!"))
                       (lambda (x val)
                         (set-car! (cdr (assoc 'field1 (cdr x))) val))) ie) ... ))))



;task 5

(define-syntax define-data
  (syntax-rules ()
    ((_ data-name ((name field1 ...) ...))
     (begin
       (eval (list 'define
                      'name
                      (lambda (field1 ...)
                        (list (list 'name-data 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...))) ie) ...
       (eval (list 'define
                      (string->symbol (string-append (symbol->string 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2) (list? (car x))
                             (let ((data-res (assoc 'name-data x)))
                               (and data-res (equal? (cadr data-res) 'data-name)))))) ie)))))

(define-syntax match
  (syntax-rules ()
    ((_ x ((name field1 ...) expr) ...)
       (cond
         ((equal? (cadadr x) 'name)
           (let ((field1 (cadr (assoc 'field1 x))) ...)
             expr))
          ...
          (else x)))))



(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

(define s #f)
(define r #f)
(define t #f)
(define c #f)

;; Приближённое значение числа Пи, рекомендуемое Архимедом.
;; См.: https://ru.wikipedia.org/wiki/Пи_(число)#История
;; Это рациональное число, оно точное (exact), с ним ошибок округления не бывает.
(define PI 22/7)

(define (perim f)
  (match f
    ((square a) (* 4 a))
    ((rectangle a b) (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r) (* 2 PI r))))

(define (scale f x)
  (match f
    ((square a) (square (* a x)))
    ((rectangle a b) (rectangle (* a x) (* b x)))
    ((triangle a b c) (triangle (* a x) (* b x) (* c x)))
    ((circle r) (circle (* r x)))))


(define-data the-list ((the-nil)
                       (the-cons head tail)))

(define (the-length xs)
  (match xs
    ((the-nil) 0)
    ((the-cons hd tl) (+ 1 (the-length tl)))))

(define (the-prod xs)
  (match xs
    ((the-nil) 1)
    ((the-cons hd tl)
     (match tl
       ((the-nil) hd)
       ((the-cons _1 _2) (* hd (the-prod tl)))))))

(define the-xs #f)
(set! the-xs (the-cons 7 (the-cons 11 (the-cons 13 (the-nil)))))
(display the-xs)
(cadadr the-xs)


(define tests
  (list (test (figure? #t) #f)
        (test (figure? #f) #f)
        (test (figure? 'hello) #f)
        (test (figure? 888) #f)
        (test (figure? '()) #f)
        (test (figure? #()) #f)
        (test (figure? '(one two three)) #f)
        (test (figure? #(one two three)) #f)

        (test (begin (set! s (square 100))
                     (figure? s))
              #t)
        (test (begin (set! r (rectangle 100 200))
                     (figure? r))
              #t)
        (test (begin (set! t (triangle 300 400 500))
                     (figure? t))
              #t)
        (test (begin (set! c (circle 100))
                     (figure? t))
              #t)

        (test (the-list? s) #f)
        (test (the-list? r) #f)
        (test (the-list? t) #f)
        (test (the-list? c) #f)

        (test (perim s) 400)
        (test (perim r) 600)
        (test (perim t) 1200)
        (test (perim c) 4400/7)

        (test (perim (scale s 1/100)) 4)
        (test (perim (scale r 1/100)) 6)
        (test (perim (scale t 1/100)) 12)
        (test (perim (scale c 1/100)) 44/7)

        (test (the-list? #t) #f)
        (test (the-list? #f) #f)
        (test (the-list? 'hello) #f)
        (test (the-list? 888) #f)
        (test (the-list? '()) #f)
        (test (the-list? #()) #f)
        (test (the-list? '(one two three)) #f)
        (test (the-list? #(one two three)) #f)

        (test (begin (set! the-xs
                           (the-cons 7 (the-cons 11 (the-cons 13 (the-nil)))))
                     (the-list? the-xs))
              #t)
        (test (figure? the-xs) #f)

        (test (the-length the-xs) 3)
        (test (the-prod the-xs) 1001)))


;(define **test-succeed-6168** (run-tests tests))

