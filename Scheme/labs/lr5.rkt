(load "unit-test.scm")

(define ie (interaction-environment))

(define math '(+ - * /))
(define compare '(= > <))
(define logic '(and or))

(define (in? xs x)
  (and (not (null? xs)) (or (equal? (car xs) x) (in? (cdr xs) x))))

(define (math-act operation stack)
  (cons (run-operation operation stack) (cddr stack)))

(define (compare-act operation stack)
  (cons (if (run-operation operation stack) -1 0) (cddr stack)))

(define (run-operation operation stack)
  (eval (list operation (cadr stack) (car stack)) ie))

(define (word-ind word program ind) 
  (if (< ind (vector-length program))
      (if (equal? (vector-ref program ind) word)
          ind
          (word-ind word program (+ ind 1)))
      #f))

(define (interpret program init-stack)
  (let loop ((ind 0) (stack init-stack) (return-stack '()) (dict '()))
    (if (= (vector-length program) ind)
        stack
        (let ((word (vector-ref program ind)))
          (cond
            ((number? word) (loop (+ ind 1) (cons word stack) return-stack dict))
            ((in? math word) (loop (+ ind 1) (math-act word stack) return-stack dict))
            ((equal? 'mod word) (loop (+ ind 1) (cons (remainder (cadr stack) (car stack)) (cddr stack)) return-stack dict))
            ((equal? 'neg word) (loop (+ ind 1) (cons (- (car stack)) (cdr stack)) return-stack dict))
            ((in? compare word) (loop (+ ind 1) (compare-act word stack) return-stack dict))
            ((equal? 'not word) (loop (+ ind 1) (cons (if (= (car stack) 0) -1 0) (cdr stack)) return-stack dict))
            ((equal? 'and word) (loop (+ ind 1) (cons (if (and (not (= (car stack) 0)) (not (= (cadr stack) 0))) -1 0) (cddr stack)) return-stack dict))
            ((equal? 'or word) (loop (+ ind 1) (cons (if (and (= (car stack) 0) (= (cadr stack) 0)) 0 -1) (cddr stack)) return-stack dict))
            ((equal? 'drop word) (loop (+ ind 1) (cdr stack) return-stack dict))
            ((equal? 'swap word) (loop (+ ind 1) (append (list (cadr stack) (car stack)) (cddr stack)) return-stack dict))
            ((equal? 'dup word) (loop (+ ind 1) (cons (car stack) stack) return-stack dict))
            ((equal? 'over word) (loop (+ ind 1) (cons (cadr stack) stack) return-stack dict))
            ((equal? 'rot word) (loop (+ ind 1) (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) return-stack dict))
            ((equal? 'depth word) (loop (+ ind 1) (cons (length stack) stack) return-stack dict))
            ((equal? 'define word) (loop (+ 1 (word-ind 'end program ind)) stack return-stack (cons (list (vector-ref program (+ ind 1)) (+ ind 2)) dict)))
            ((in? '(exit end) word) (loop (car return-stack) stack (cdr return-stack) dict))
            ((equal? 'if word) (if (word-ind 'else program ind)
                                   (loop (if (zero? (car stack)) (+ 1 (word-ind 'else program ind)) (+ ind 1)) (cdr stack) return-stack dict)
                                   (loop (if (zero? (car stack)) (+ 1 (word-ind 'endif program ind)) (+ ind 1)) (cdr stack) return-stack dict)))
            ((equal? 'endif word) (loop (+ ind 1) stack return-stack dict))
            (else (loop (cadr (assoc word dict)) stack (cons (+ ind 1) return-stack) dict)))))))

(define interpret-tests
  (list
   (test (interpret #(   define -- 1 - end
                5 -- --      ) '()) '(3))
   (test (interpret #(   define abs
                          dup 0 <
                          if neg endif
                          end
                          9 abs
                          -9 abs      ) (quote ())) '(9 9))
   (test (interpret #(   define =0? dup 0 = end
                          define <0? dup 0 < end
                          define signum
                          =0? if exit endif
                          <0? if drop -1 exit endif
                          drop
                          1
                          end
                          0 signum
                          -5 signum
                          10 signum       ) (quote ())) '(1 -1 0))
   (test (interpret #(   define -- 1 - end
                          define =0? dup 0 = end
                          define =1? dup 1 = end
                          define factorial
                          =0? if drop 1 exit endif
                          =1? if drop 1 exit endif
                          dup --
                          factorial
                          *
                          end
                          0 factorial
                          1 factorial
                          2 factorial
                          3 factorial
                          4 factorial     ) (quote ())) '(24 6 2 1 1))
   (test (interpret #(   define =0? dup 0 = end
                          define =1? dup 1 = end
                          define -- 1 - end
                          define fib
                          =0? if drop 0 exit endif
                          =1? if drop 1 exit endif
                          -- dup
                          -- fib
                          swap fib
                          +
                          end
                          define make-fib
                          dup 0 < if drop exit endif
                          dup fib
                          swap --
                          make-fib
                          end
                          10 make-fib     ) (quote ())) '(0 1 1 2 3 5 8 13 21 34 55))
   (test (interpret #(   define =0? dup 0 = end
                          define gcd
                          =0? if drop exit endif
                          swap over mod
                          gcd
                          end
                          90 99 gcd
                          234 8100 gcd    ) '()) '(18 9))
   ))

(run-tests interpret-tests)
(newline)
            