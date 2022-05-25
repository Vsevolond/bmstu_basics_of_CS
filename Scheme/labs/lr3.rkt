;task 1
(load "trace.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))

(display "(zip '(1 2 3) '(one two three))")
(newline)
(zip '(1 2 3) '(one two three))

(define counter
  (let ((c 0))
    (lambda ()
      (set! c (+ 1 c))
      c)))


;task 2
(load "unit-test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)
        
      


;task 3
(define (to-list x)
  (cond ((string? x) (string->list x))
        ((vector? x) (vector->list x))
        (else x)))

#|(define (ref xs n)
  (define (loop x i)
    (if (or (null? x) (< i 0))
        #f
        (if (= i 0)
            (car x)
            (loop (cdr x) (- i 1)))))
  (loop (to-list xs) n))|#
  
(define (ref m . xs)
  (define (ref1 x i)
    (if (or (null? x) (< i 0))
        #f
        (if (= i 0)
            (car x)
            (ref1 (cdr x) (- i 1)))))
  (define (ref2 x i)
    (if (= i 0)
        (cons (cadr xs) x)
        (cons (car x) (ref2 (cdr x) (- i 1)))))
  (cond ((= (length xs) 1)
         (ref1 (to-list m) (car xs)))
        ((= (length xs) 2)
         (cond ((or (> (car xs) (length (to-list m))) (< (car xs) 0) (and (string? m) (not (char? (cadr xs))))) #f)
               ((and (string? m) (char? (cadr xs))) (list->string (ref2 (to-list m) (car xs))))
               ((vector? m) (list->vector (ref2 (to-list m) (car xs))))
               (else (ref2 (to-list m) (car xs)))))))
         
#|(define (ref xs n a)
  (define (loop x i)
    (if (= i 0)
        (cons a x)
        (cons (car x) (loop (cdr x) (- i 1)))))
  (cond ((or (> n (length (to-list xs))) (< n 0) (and (string? xs) (not (char? a)))) #f)
        ((and (string? xs) (char? a)) (list->string (loop (to-list xs) n)))
        ((vector? xs) (list->vector (loop (to-list xs) n)))
        (else (loop (to-list xs) n))))|#

(define test3
  (list (test (ref '(1 2 3) 1) 2)
        (test (ref #(1 2 3) 1) 2)
        (test (ref "123" 1) #\2)
        (test (ref "123" 3) #f)
        (test (ref '(1 2 3) 1 0) '(1 0 2 3))
        (test (ref #(1 2 3) 1 0)  #(1 0 2 3))
        (test (ref #(1 2 3) 1 #\0) #(1 #\0 2 3))
        (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0) #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)))

(run-tests test3)
  


;task 4
(define (factorize exmpl)
  (define a (cadr exmpl))
  (define b (caddr exmpl))
  (cond ((equal? (caddr a) 2)
         `(* (- ,(cadr a) ,(cadr b)) (+ ,(cadr a) ,(cadr b))))
        ((equal? (car exmpl) -)
         `(* (- ,(cadr a) ,(cadr b)) (+ (expt ,(cadr a) 2) (* ,(cadr a) ,(cadr b)) (expt ,(cadr b) 2))))
        ((equal? (car exmpl) +)
         `(* (+ ,(cadr a) ,(cadr b)) (+ (- (expt ,(cadr a) 2) (* ,(cadr a) ,(cadr b))) (expt ,(cadr b) 2))))))

                                      
(define test4
  (list
   (test (factorize '(- (expt x 2) (expt y 2))) '(* (- x y) (+ x y)))
   (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) '(* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
   (test (eval (list (list 'lambda 
                     '(x y) 
                     (factorize '(- (expt x 2) (expt y 2))))
               1 2)
         (interaction-environment)) -3)))

(run-tests test4)