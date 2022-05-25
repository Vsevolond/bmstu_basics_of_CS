(define-syntax test
  (syntax-rules ()
    ((test expr res)
     (list (quote expr) (lambda () expr) res))))

(define (run-test xs)
  (display (car xs))
  (let ((x1 ((cadr xs))) (x2 (caddr xs)))
    (if (equal? x1 x2)
        (begin
          (display " ok")
          (newline) 
          #t)
        (begin
          (display " FAIL")
          (newline)
          (display "  Expected: ")
          (display x2)
          (newline)
          (display "  Returned: ")
          (display x1)
          (newline)
          #f))))

(define (run-tests lst)
  (define (loop xs f)
    (if (null? xs)
        f
        (loop (cdr xs) (and (run-test (car xs)) f))))
  (loop lst #t))