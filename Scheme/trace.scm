(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex x)
     (begin
       (display 'x)
       (display " => ")
       (let ((x2 x))
         (display x2)
         (newline)
         x2)))))