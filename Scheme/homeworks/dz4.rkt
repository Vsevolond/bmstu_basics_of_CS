

;task 1

(define memoized-factorial
  (let loop ((known-results '()))
    (lambda (n)
      (if (<= n 1)
          1
          (let ((res (assoc n known-results)))
            (if res
                (cadr res)
                (let ((res (* n (memoized-factorial (- n 1)))))
                  (set! known-results (cons (list n res) known-results))
                  res)))))))


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

(display "test (lazy-head):") (newline)

(define lazy-head-test
  (list
   (test (lazy-head (naturals 10) 12) '(10 11 12 13 14 15 16 17 18 19 20 21))))

(run-tests lazy-head-test)

(define (factorials)
  (let loop ((prime 1) (num 1))
    (lazy-cons (* prime num) (loop (* prime num) (+ num 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n)
            (- n 1)))



;task 3

(define (read-words)
  (let loop ((words '()) (str ""))
    (if (eof-object? (peek-char))
        (reverse words)
        (let ((c (read-char)))
          (if (char-whitespace? c)
              (if (> (string-length str) 0)
                  (loop (cons str words) "")
                  (loop words ""))
              (loop words (string-append str (string c))))))))



;task 4

(define ie (interaction-environment))
(define (str<->symb s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (field1 ...))
     (begin
       (eval (list 'define
                      (str<->symb (string-append "make-" (str<->symb 'name)))
                      (lambda (field1 ...)
                        (list (list 'type 'name) (list 'field1 field1) ...))) ie)
       (eval (list 'define
                      (str<->symb (string-append (str<->symb 'name) "?"))
                      (lambda (x)
                        (and (list? x) (not (null? x))
                             (let ((ares (assoc 'type x)))
                               (and ares (equal? (cadr ares) 'name)))))) ie)
       (eval (list 'define
                       (str<->symb (string-append (str<->symb 'name) "-" (str<->symb 'field1)))
                       (lambda (x)
                         (cadr (assoc 'field1 (cdr x))))) ie) ...
       (eval (list 'define
                       (str<->symb (string-append "set-" (str<->symb 'name) "-" (str<->symb 'field1) "!"))
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
                        (list (list 'd-name 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...))) ie) ...
       (eval (list 'define
                      (str<->symb (string-append (str<->symb 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2)
                             (let ((data-res (assoc 'd-name x)))
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

