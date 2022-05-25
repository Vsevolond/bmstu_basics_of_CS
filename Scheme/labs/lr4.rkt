(load "unit-test.scm")

;task 1

(define call/cc call-with-current-continuation)
(define ie (interaction-environment))

(define r #f)

(define-syntax use-assertions
  (syntax-rules ()
    ((_) (call/cc
          (lambda (c)
            (set! r c))))))

(define-syntax assert
  (syntax-rules ()
    ((_ expr) (if (eval expr ie)
                  #t
                  (begin
                    (display "FAILED:")
                    (r (write 'expr)))))))

(use-assertions) 

(define (1/x x)
  (assert (not (zero? x))) 
  (/ 1 x))

(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))
(newline)


;task 2

(define (save-data data file)
  (call-with-output-file file
    (lambda (port)
      (write data port))))

(define (load-data file)
  (call-with-input-file file
    (lambda (port)
      (read port))))
   
(define data (load-data "/Users/vsevolod/Documents/Scheme/input.txt"))
(display data)
(newline)
;(save-data data "/Users/vsevolod/Documents/Scheme/output.txt")

(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define (read-line cnt)
        (define not-empty #f)
        (define (read-str c)
          (if (not (or (eof-object? c) (char-whitespace? c)))
              (begin
                (set! not-empty #t)
                (read-str (read-char port)))
              (if (or (equal? c #\space) (equal? c #\tab))
                  (read-str (read-char port))
                  (eof-object? c))))
        (if (equal? (read-str (read-char port)) #t)
            (if (equal? not-empty #t)
                (+ cnt 1)
                cnt)
            (if (equal? not-empty #t)
                (read-line (+ cnt 1))
                (read-line cnt))))
      (read-line 0))))

(count-line "lr4.rkt")


;task 3

(define (trib n)
  (if (<= n 1)
      0
      (if (= n 2)
          1
          (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3))))))

(define trib-memo
  (let ((known-results '()))
    (lambda (n)
      (let* ((arg n)
             (res (assoc arg known-results)))
        (if res
            (cadr res)
            (let ((res (if (<= n 1)
                           0
                           (if (= n 2)
                               1
                               (+ (trib-memo (- n 1)) (trib-memo (- n 2)) (trib-memo (- n 3)))))))
              (set! known-results (cons (list arg res) known-results))
              res))))))

(define trib-tests
  (list
   (test (trib 1) (trib-memo 1))
   (test (trib 2) (trib-memo 2))
   (test (trib 3) (trib-memo 3))
   (test (trib 4) (trib-memo 4))
   (test (trib 7) (trib-memo 7))
   (test (trib 10) (trib-memo 10))))

(run-tests trib-tests)

;
;task 4

(define-syntax my-if
  (syntax-rules ()
    ((my-if cond? t-expr f-expr)
     (let ((t-promise (delay t-expr))
           (f-promise (delay f-expr)))
       (force (or (and cond? t-promise) f-promise))))))

(define my-if-tests
  (list
   (test (my-if #t 1 (/ 1 0)) 1)
   (test (my-if #f (/ 1 0) 1) 1)))

(run-tests my-if-tests)


;task 5

(define-syntax my-let
  (syntax-rules ()
    ((my-let ((name1 value1) ...) expr1 ...)
     ((lambda (name1 ...) expr1 ...) value1 ...))))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () expr1 ...)
     (my-let () expr1 ...))
    ((my-let* ((name1 value1)) expr1 ...)
     (my-let ((name1 value1)) expr1 ...))
    ((my-let* ((name1 value1) (name2 value2) ...) expr1 ...)
     (my-let ((name1 value1))
             (my-let* ((name2 value2) ...) expr1 ...)))))

(define let-tests
  (list
   (test (my-let* () 10) 10)
   (test (my-let* ((x 100)) x) 100)
   (test (my-let* ((x 10) (y (* x 10))) (+ x y)) 110)))

(run-tests let-tests)


;task 6

; А

(define-syntax when
  (syntax-rules ()
    ((when cond? expr1 ...)
     (if cond? (begin expr1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless cond? expr1 ...)
     (if (not cond?) (begin expr1 ...)))))

(display "test (when) and (unless):") (newline)
(define x 1)
(when   (> x 0) (display "x > 0") (newline))
(unless (< x 0) (display "x >= 0") (newline))


; Б

(define-syntax for
  (syntax-rules (in as)
    ((for i in xs expr1 ...)
     (let loop ((var xs))
       (if (not (null? var))
           (let ((i (car var)))
             expr1 ...
             (loop (cdr var))))))
    ((for xs as i expr1 ...) (for i in xs expr1 ...))))

(display "test (for):") (newline)
(for i in '(a b c)
  (for j in '(x y z)
    (display (list i j))
    (newline)))

(for '(a b c) as i
  (for '(x y z) as j
    (display (list i j))
    (newline)))
           

; В

(define-syntax while
  (syntax-rules ()
    ((while cond body ...)
     (let loop ()
       (when cond
         body ...
         (loop))))))

(display "test (while):") (newline)

(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))


; Г

(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr1 ...) until cond?)
     (let loop ()
       expr1 ...
       (if (not cond?) (loop))))))


(display "test (repeat until):") (newline)

(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))


; Д

(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (newline))
    ((cout << endl . exprn)
     (begin (newline)
            (cout . exprn)))
    ((cout << expr1)
     (display expr1))
    ((cout << expr1 . exprn)
     (begin (display expr1)
            (cout . exprn)))))


(display "test (cout):")
(cout << endl << "a = " << 1 << endl << "b = " << 2 << endl)