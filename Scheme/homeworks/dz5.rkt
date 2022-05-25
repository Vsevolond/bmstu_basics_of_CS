
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

(define (jump-if program index)
  (let loop ((cnt-if 1) (ind index))
    (if (zero? cnt-if)
        ind
        (if (word-ind 'if program ind)
            (loop (+ cnt-if 1) (+ 1 (word-ind 'if program ind)))
            (if (and (= cnt-if 1) (word-ind 'else program ind))
                (+ 1 (word-ind 'else program ind))
                (loop (- cnt-if 1) (+ 1 (word-ind 'endif program ind))))))))

(define (first-exit program ind)
  (let ((ind1 (word-ind 'wend program ind)) (ind2 (word-ind 'repeat program ind)))
    (cond
      ((and ind1 ind2) (if (< ind1 ind2) ind1 ind2))
      ((and ind1 (not ind2)) ind1)
      ((and (not ind1) ind2) ind2))))
        
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
            ((equal? 'if word) (if (word-ind 'if program (+ ind 1))
                                   (if (zero? (car stack))
                                        (loop (jump-if program (+ ind 1)) (cdr stack) return-stack dict)
                                        (loop (+ ind 1) (cdr stack) return-stack dict))
                                   (if (word-ind 'else program ind)
                                       (loop (if (zero? (car stack)) (+ 1 (word-ind 'else program ind)) (+ ind 1)) (cdr stack) return-stack dict)
                                       (loop (if (zero? (car stack)) (+ 1 (word-ind 'endif program ind)) (+ ind 1)) (cdr stack) return-stack dict))))
            ((equal? 'else word) (if (zero? (car stack))
                                     (loop (+ ind 1) (cdr stack) return-stack dict)
                                     (loop (+ 1 (word-ind 'endif program ind)) stack return-stack dict)))
            ((equal? 'endif word) (loop (+ ind 1) stack return-stack dict))
            ((equal? 'while word) (if (zero? (car stack))
                                      (loop (+ 1 (word-ind 'wend program ind)) (cdr stack) return-stack dict)
                                      (loop (+ ind 1) (cdr stack) (cons ind return-stack) dict)))
            ((equal? 'wend word) (loop (car return-stack) stack (cdr return-stack) dict))
            ((equal? 'repeat word) (loop (+ ind 1) stack (cons ind return-stack) dict))
            ((equal? 'until word) (loop (if (zero? (car stack)) (+ ind 1) (car return-stack)) (cdr stack) (cdr return-stack) dict))
            ((equal? 'break word) (loop (+ 1 (first-exit program ind)) stack return-stack dict))
            ((equal? 'continue word) (loop (first-exit program ind) stack return-stack dict))
            ((equal? 'switch word) (
            (else (loop (cadr (assoc word dict)) stack (cons (+ ind 1) return-stack) dict)))))))

