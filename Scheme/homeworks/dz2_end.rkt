(define (string-trim-left s)
  (if (= (string-length s) 0)
      ""
      (if (and (= (string-length s) 1) (char-whitespace? (string-ref s 0)))
          ""
          (if (and (> (string-length s) 1) (char-whitespace? (string-ref s 0)))
              (string-trim-left (substring s 1 (string-length s)))
              s))))

(display "(string-trim-left \"\\t\\tabc def\") -> ")
(string-trim-left "\t\tabc def")


(define (string-trim-right s)
  (if (= (string-length s) 0)
      ""
      (if (and (= (string-length s) 1) (char-whitespace? (string-ref s (- (string-length s) 1))))
          ""
          (if (and (> (string-length s) 1) (char-whitespace? (string-ref s (- (string-length s) 1))))
              (string-trim-right (substring s 0 (- (string-length s) 1)))
              s))))

(display "(string-trim-right \"abc def\\t\") -> ")
(string-trim-right "abc def\t")


(define (string-trim s)
  (string-trim-left (string-trim-right s)))

(display "(string-trim       \"\\t abc def \\n\") -> ")
(string-trim       "\t abc def \n")


(define (string-prefix? a b)
  (if (< (string-length b) (string-length a))
      #f
      (if (equal? a b)
          #t
          (string-prefix? a (substring b 0 (- (string-length b) 1))))))

(display "(string-prefix? \"abc\" \"abcdef\") -> ")
(string-prefix? "abc" "abcdef")
(display "(string-prefix? \"bcd\" \"abcdef\") -> ")
(string-prefix? "bcd" "abcdef")
(display "(string-prefix? \"abcdef\" \"abc\") -> ")
(string-prefix? "abcdef" "abc")


(define (string-suffix? a b)
  (if (or (= (string-length a) 0) (= (string-length b) 0))
      #t
      (if (equal? (string-ref a (- (string-length a) 1)) (string-ref b (- (string-length b) 1)))
          (string-suffix? (substring a 0 (- (string-length a) 1)) (substring b 0 (- (string-length b) 1)))
          #f)))

(display "(string-suffix? \"def\" \"abcdef\") -> ")
(string-suffix? "def" "abcdef")
(display "(string-suffix? \"bcd\" \"abcdef\") -> ")
(string-suffix? "bcd" "abcdef")


(define (string-infix? a b)
  (define (f i j)
    (if (> j (string-length b))
        #f
        (if (equal? a (substring b i j))
            #t
            (f (+ i 1) (+ j 1)))))
  (f 0 (string-length a)))

(display "(string-infix? \"def\" \"abcdefgh\") -> ")
(string-infix? "def" "abcdefgh")
(display "(string-infix? \"abc\" \"abcdefgh\") -> ")
(string-infix? "abc" "abcdefgh")
(display "(string-infix? \"fgh\" \"abcdefgh\") -> ")
(string-infix? "fgh" "abcdefgh")
(display "(string-infix? \"ijk\" \"abcdefgh\") -> ")
(string-infix? "ijk" "abcdefgh")
(display "(string-infix? \"bcd\" \"abc\") -> ")
(string-infix? "bcd" "abc")


(define (string-split str sep)
  (define (f i j)
    (if (or (= j (string-length str)) (> (+ j (string-length sep)) (string-length str)))
        (cons (substring str i (string-length str)) (list))
        (if (equal? (substring str j (+ j (string-length sep))) sep)
            (cons (substring str i j) (f (+ j (string-length sep)) (+ j (string-length sep))))
            (f i (+ j 1)))))
  (f 0 0))

(display "(string-split \"x;y;z\" \";\") -> ")
(string-split "x;y;z" ";")
(display "(string-split \"x-->y-->z\" \"-->\") -> ")
(string-split "x-->y-->z" "-->")


(define (make-multi-vector . xs)
  (if (> (length xs) 1)
      (list (car xs) (make-vector (apply * (car xs)) (cadr xs)))
      (list (car xs) (make-vector (apply * (car xs))))))


(define (multi-vector? m)
  (and (list? m)
       (and (list? (car m)) (vector? (cadr m)))))


(define (multi-vector-ref m indices)
  (define (pos ind len)
    (if (= (length ind) 1)
        (car ind)
        (+ (* (car ind) (apply * (cdr len))) (pos (cdr ind) (cdr len)))))
  (vector-ref (cadr m) (pos indices (car m))))


(define (multi-vector-set! m indices x)
  (define (pos ind len)
    (if (= (length ind) 1)
        (car ind)
        (+ (* (car ind) (apply * (cdr len))) (pos (cdr ind) (cdr len)))))
  (vector-set! (cadr m) (pos indices (car m)) x))


(define m (make-multi-vector '(11 12 9 16)))
(display "(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m) -> ")
(multi-vector? m)
(display "(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12)) -> ")
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12))

(display "(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)) ⇒ ")
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1))
(display "(multi-vector-ref m '(2 1 1 1)) ⇒ ")
(multi-vector-ref m '(2 1 1 1))

(display "(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0)) ⇒ ")
(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))


(define m3 (make-multi-vector '(2 2 2)))
(multi-vector-set! m3 '(0 0 0) 1000)
(multi-vector-set! m3 '(0 0 1) 1001)
(multi-vector-set! m3 '(0 1 0) 1010)
(multi-vector-set! m3 '(1 0 0) 1100)

(multi-vector-ref m3 '(0 0 0))
(multi-vector-ref m3 '(0 0 1))
(multi-vector-ref m3 '(0 1 0))
(multi-vector-ref m3 '(1 0 0))


(define (list-trim-right xs)
  (string->list (string-trim-right (list->string xs))))

(string->list "\t abc def \n")
(list-trim-right (string->list "\t abc def \n"))
          