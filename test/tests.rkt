#lang racket

(provide make-test-suite
         make-io-test-suite
         run-tests)

(require rackunit
         rackunit/text-ui)

(define (make-test-suite name run-proc)
  (test-suite
   name

   ;; Abscond examples
   (test-suite
    "Abscond"
    (check-equal? (run-proc 7) 7)
    (check-equal? (run-proc -8) -8))

   ;; Blackmail examples
   (test-suite
    "Blackmail"
    (check-equal? (run-proc '(add1 (add1 7))) 9)
    (check-equal? (run-proc '(add1 (sub1 7))) 7))

   ;; Con examples
   (test-suite
    "Con"
    (check-equal? (run-proc '(if (zero? 0) 1 2)) 1)
    (check-equal? (run-proc '(if (zero? 1) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? -7) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? 0)
                                 (if (zero? 1) 1 2)
                                 7))
                  2)
    (check-equal? (run-proc '(if (zero? (if (zero? 0) 1 0))
                                 (if (zero? 1) 1 2)
                                 7))
                  7))

   ;; Dupe examples
   (test-suite
    "Dupe"
    (check-equal? (run-proc #t) #t)
    (check-equal? (run-proc #f) #f)
    (check-equal? (run-proc '(if #t 1 2)) 1)
    (check-equal? (run-proc '(if #f 1 2)) 2)
    (check-equal? (run-proc '(if 0 1 2)) 1)
    (check-equal? (run-proc '(if #t 3 4)) 3)
    (check-equal? (run-proc '(if #f 3 4)) 4)
    (check-equal? (run-proc '(if 0 3 4)) 3)
    (check-equal? (run-proc '(zero? 4)) #f)
    (check-equal? (run-proc '(zero? 0)) #t))

   ;; Dodger examples
   (test-suite
    "Dodger"
    (check-equal? (run-proc #\a) #\a)
    (check-equal? (run-proc #\b) #\b)
    (check-equal? (run-proc '(char? #\a)) #t)
    (check-equal? (run-proc '(char? #t)) #f)
    (check-equal? (run-proc '(char? 8)) #f)
    (check-equal? (run-proc '(char->integer #\a)) (char->integer #\a))
    (check-equal? (run-proc '(integer->char 955)) #\λ))

   ;; Evildoer examples
   (test-suite
    "Evildoer"
    (check-equal? (run-proc '(void)) (void))
    (check-equal? (run-proc '(begin 1 2)) 2)
    (check-equal? (run-proc '(eof-object? (void))) #f))

   ;; Extort examples
   (test-suite
    "Extort"
    (check-equal? (run-proc '(add1 #f)) 'err)
    (check-equal? (run-proc '(sub1 #f)) 'err)
    (check-equal? (run-proc '(zero? #f)) 'err)
    (check-equal? (run-proc '(char->integer #f)) 'err)
    (check-equal? (run-proc '(integer->char #f)) 'err)
    (check-equal? (run-proc '(integer->char -1)) 'err)
    (check-equal? (run-proc '(write-byte #f)) 'err)
    (check-equal? (run-proc '(write-byte -1)) 'err)
    (check-equal? (run-proc '(write-byte 256)) 'err)
    (check-equal? (run-proc '(begin (integer->char 97)
                                    (integer->char 98)))
                  #\b))

   ;; Fraud examples
   (test-suite
    "Fraud"
    (check-equal? (run-proc '(let ([x 7]) x)) 7)
    (check-equal? (run-proc '(let ([x 7]) 2)) 2)
    (check-equal? (run-proc '(let ([x 7]) (add1 x))) 8)
    (check-equal? (run-proc '(let ([x (add1 7)]) x)) 8)
    (check-equal? (run-proc '(let ([x 7]) (let ((y 2)) x))) 7)
    (check-equal? (run-proc '(let ([x 7]) (let ((x 2)) x))) 2)
    (check-equal? (run-proc '(let ([x 7]) (let ((x (add1 x))) x))) 8)
    (check-equal? (run-proc '(let ([x 0])
                               (if (zero? x) 7 8)))
                  7)
    (check-equal? (run-proc '(let ([x 1])
                               (add1 (if (zero? x) 7 8))))
                  9)
    (check-equal? (run-proc '(+ 3 4)) 7)
    (check-equal? (run-proc '(- 3 4)) -1)
    (check-equal? (run-proc '(+ (+ 2 1) 4)) 7)
    (check-equal? (run-proc '(+ (+ 2 1) (+ 2 2))) 7)
    (check-equal? (run-proc '(let ([x (+ 1 2)])
                               (let ([z (- 4 x)])
                                 (+ (+ x x) z))))
                  7)
    (check-equal? (run-proc '(= 5 5)) #t)
    (check-equal? (run-proc '(= 4 5)) #f)
    (check-equal? (run-proc '(= (add1 4) 5)) #t)
    (check-equal? (run-proc '(< 5 5)) #f)
    (check-equal? (run-proc '(< 4 5)) #t)
    (check-equal? (run-proc '(< (add1 4) 5)) #f))

   ;; Hustle examples
   (test-suite
    "Hustle"
    (check-equal? (run-proc ''()) '())
    (check-equal? (run-proc '(empty? '())) #t)
    (check-equal? (run-proc '(empty? 3)) #f)
    (check-equal? (run-proc '(empty? (cons 1 2))) #f)
    (check-equal? (run-proc '(box 1)) (box 1))
    (check-equal? (run-proc '(box -1)) (box -1))
    (check-equal? (run-proc '(cons 1 2)) (cons 1 2))
    (check-equal? (run-proc '(unbox (box 1))) 1)
    (check-equal? (run-proc '(car (cons 1 2))) 1)
    (check-equal? (run-proc '(cdr (cons 1 2))) 2)
    (check-equal? (run-proc '(cons 1 '())) (list 1))
    (check-equal? (run-proc '(let ((x (cons 1 2)))
                               (begin (cdr x)
                                      (car x))))
                  1)
    (check-equal? (run-proc '(let ((x (cons 1 2)))
                               (let ((y (box 3)))
                                 (unbox y))))
                  3)
    (check-equal? (run-proc '(eq? 1 1)) #t)
    (check-equal? (run-proc '(eq? 1 2)) #f)
    (check-equal? (run-proc '(eq? (cons 1 2) (cons 1 2))) #f)
    (check-equal? (run-proc '(let ((x (cons 1 2))) (eq? x x))) #t))

   ;; Hoax examples
   (test-suite
    "Hoax"
    (check-equal? (run-proc '(make-vector 0 0)) #())
    (check-equal? (run-proc '(make-vector 1 0)) #(0))
    (check-equal? (run-proc '(make-vector 3 0)) #(0 0 0))
    (check-equal? (run-proc '(make-vector 3 5)) #(5 5 5))
    (check-equal? (run-proc '(vector? (make-vector 0 0))) #t)
    (check-equal? (run-proc '(vector? (cons 0 0))) #f)
    (check-equal? (run-proc '(vector-ref (make-vector 0 #f) 0)) 'err)
    (check-equal? (run-proc '(vector-ref (make-vector 3 5) -1)) 'err)
    (check-equal? (run-proc '(vector-ref (make-vector 3 5) 0)) 5)
    (check-equal? (run-proc '(vector-ref (make-vector 3 5) 1)) 5)
    (check-equal? (run-proc '(vector-ref (make-vector 3 5) 2)) 5)
    (check-equal? (run-proc '(vector-ref (make-vector 3 5) 3)) 'err)
    (check-equal? (run-proc '(let ((x (make-vector 3 5)))
                               (begin (vector-set! x 0 4)
                                      x)))
                  #(4 5 5))
    (check-equal? (run-proc '(let ((x (make-vector 3 5)))
                               (begin (vector-set! x 1 4)
                                      x)))
                  #(5 4 5))
    (check-equal? (run-proc '(vector-length (make-vector 3 #f))) 3)
    (check-equal? (run-proc '(vector-length (make-vector 0 #f))) 0)
    (check-equal? (run-proc '"") "")
    (check-equal? (run-proc '"fred") "fred")
    (check-equal? (run-proc '"wilma") "wilma")
    (check-equal? (run-proc '(make-string 0 #\f)) "")
    (check-equal? (run-proc '(make-string 3 #\f)) "fff")
    (check-equal? (run-proc '(make-string 3 #\g)) "ggg")
    (check-equal? (run-proc '(string-length "")) 0)
    (check-equal? (run-proc '(string-length "fred")) 4)
    (check-equal? (run-proc '(string-ref "" 0)) 'err)
    (check-equal? (run-proc '(string-ref (make-string 0 #\a) 0)) 'err)
    (check-equal? (run-proc '(string-ref "fred" 0)) #\f)
    (check-equal? (run-proc '(string-ref "fred" 1)) #\r)
    (check-equal? (run-proc '(string-ref "fred" 2)) #\e)
    (check-equal? (run-proc '(string-ref "fred" 4)) 'err)
    (check-equal? (run-proc '(string? "fred")) #t)
    (check-equal? (run-proc '(string? (cons 1 2))) #f)
    (check-equal? (run-proc '(begin (make-string 3 #\f)
                                    (make-string 3 #\f)))
                  "fff"))

   ;; Iniquity examples
   (test-suite
    "Iniquity"
    (check-equal? (run-proc
                   '(define (f x) x)
                   '(f 5))
                  5)
    (check-equal? (run-proc
                   '(define (tri x)
                      (if (zero? x)
                          0
                          (+ x (tri (sub1 x)))))
                   '(tri 9))
                  45)

    (check-equal? (run-proc
                   '(define (even? x)
                      (if (zero? x)
                          #t
                          (odd? (sub1 x))))
                   '(define (odd? x)
                      (if (zero? x)
                          #f
                          (even? (sub1 x))))
                   '(even? 101))
                  #f)

    (check-equal? (run-proc
                   '(define (map-add1 xs)
                      (if (empty? xs)
                          '()
                          (cons (add1 (car xs))
                                (map-add1 (cdr xs)))))
                   '(map-add1 (cons 1 (cons 2 (cons 3 '())))))
                  '(2 3 4))
    (check-equal? (run-proc '(define (f x y) y)
                            '(f 1 (add1 #f)))
                  'err))

   ;; Knock examples
   (test-suite
    "Knock"
    (check-equal? (run-proc '(match 1)) 'err)
    (check-equal? (run-proc '(match 1 [1 2]))
                  2)
    (check-equal? (run-proc '(match 1 [2 1] [1 2]))
                  2)
    (check-equal? (run-proc '(match 1 [2 1] [1 2] [0 3]))
                  2)
    (check-equal? (run-proc '(match 1 [2 1] [0 3]))
                  'err)
    (check-equal? (run-proc '(match 1 [_ 2] [_ 3]))
                  2)
    (check-equal? (run-proc '(match 1 [x 2] [_ 3]))
                  2)
    (check-equal? (run-proc '(match 1 [x x] [_ 3]))
                  1)
    (check-equal? (run-proc '(match (cons 1 2) [x x] [_ 3]))
                  (cons 1 2))
    (check-equal? (run-proc '(match (cons 1 2) [(cons x y) x] [_ 3]))
                  1)
    (check-equal? (run-proc '(match (cons 1 2) [(cons x 2) x] [_ 3]))
                  1)
    (check-equal? (run-proc '(match (cons 1 2) [(cons 3 2) 0] [_ 3]))
                  3)
    (check-equal? (run-proc '(match 1 [(cons x y) x] [_ 3]))
                  3)
    (check-equal? (run-proc '(match (cons 1 2) [(cons 1 3) 0] [(cons 1 y) y] [_ 3]))
                  2)
    (check-equal? (run-proc '(match (box 1) [(box 1) 0] [_ 1]))
                  0)
    (check-equal? (run-proc '(match (box 1) [(box 2) 0] [_ 1]))
                  1)
    (check-equal? (run-proc '(match (box 1) [(box x) x] [_ 2]))
                  1))

   ;; Loot examples
   (test-suite
    "Loot"
    (check-true (procedure? (run-proc '(λ (x) x))))
    (check-equal? (run-proc '((λ (x) x) 5))
                  5)
    (check-equal? (run-proc '(let ((f (λ (x) x))) (f 5)))
                  5)
    (check-equal? (run-proc '(let ((f (λ (x y) x))) (f 5 7)))
                  5)
    (check-equal? (run-proc '(let ((f (λ (x y) y))) (f 5 7)))
                  7)
    (check-equal? (run-proc '((let ((x 1))
                                (let ((y 2))
                                  (lambda (z) (cons x (cons y (cons z '()))))))
                              3))
                  '(1 2 3))
    (check-equal? (run-proc '(define (adder n)
                               (λ (x) (+ x n)))
                            '((adder 5) 10))
                  15)
    (check-equal? (run-proc '(((λ (t)
                                 ((λ (f) (t (λ (z) ((f f) z))))
                                  (λ (f) (t (λ (z) ((f f) z))))))
                               (λ (tri)
                                 (λ (n)
                                   (if (zero? n)
                                       0
                                       (+ n (tri (sub1 n)))))))
                              36))
                  666)
    (check-equal? (run-proc '(define (tri n)
                               (if (zero? n)
                                   0
                                   (+ n (tri (sub1 n)))))
                            '(tri 36))
                  666)
    (check-equal? (run-proc '(define (tri n)
                               (match n
                                 [0 0]
                                 [m (+ m (tri (sub1 m)))]))
                            '(tri 36))
                  666)
    (check-equal? (run-proc '((match 8 [8 (lambda (x) x)]) 12))
                  12))

   ;; Loot+ examples
   (test-suite
    "Loot+"
    (check-true (procedure? (run-proc '(λ xs xs))))
    (check-equal? (run-proc '(define (f x) x)
                            '(f))
                  'err)
    (check-equal? (run-proc '(define (f) 1)
                            '(f 2))
                  'err)
    (check-equal? (run-proc '(define (f x) x)
                            '(let ((y 2))
                               (f 1 y)))
                  'err)
    (check-equal? (run-proc '(define (f . xs)
                               (if (empty? xs)
                                   #t
                                   (f)))
                            '(f 1 2 3))
                  #t)
    (check-equal? (run-proc '(define (list . xs) xs)
                            '(list (list) (list 1 2 3) (list #t) (list 3 4 5)))
                  '(() (1 2 3) (#t) (3 4 5)))
    (check-equal? (run-proc '(define (f x y . z) (cons x (cons y z)))
                            '(cons (f 1 2) (cons (f 8 9 10) '())))
                  '((1 2) (8 9 10)))
    (check-equal? (run-proc '(define (f x . xs) x)
                            '(f 1))
                  1)
    (check-equal? (run-proc '(define (f x . xs) xs)
                            '(f 1))
                  '())
    (check-equal? (run-proc '(define (f x . xs) xs)
                            '(f))
                  'err)
    (check-equal? (run-proc '(define (f x . xs) xs)
                            '(let ((x 3))
                               (f 1 x)))
                  '(3))
    (check-equal? (run-proc '(define (f) 1)
                            '(apply f '()))
                  1)
    (check-equal? (run-proc '(define (f . xs) 1)
                            '(apply f '()))
                  1)
    (check-equal? (run-proc '(define (f . xs) xs)
                            '(apply f '()))
                  '())
    (check-equal? (run-proc '(define (f . xs) xs)
                            '(apply f (cons 1 (cons 2 (cons 3 '())))))
                  '(1 2 3))
    (check-equal? (run-proc '(define (f . xs) xs)
                            '(apply f 1 2 (cons 3 '())))
                  '(1 2 3))
    (check-equal? (run-proc '(define (append . xss)
                               (if (empty? xss)
                                   '()
                                   (if (empty? (car xss))
                                       (apply append (cdr xss))
                                       (cons (car (car xss))
                                             (apply append (cdr (car xss)) (cdr xss))))))
                            '(define (list . xs) xs)
                            '(define (flatten xs)
                               (apply append xs))
                            '(flatten (list (append) (append (list 1 2 3) (list 4 5) (list 6)) (list 7))))
                  '(1 2 3 4 5 6 7))
    (check-equal? (run-proc '(define (f . xs) xs)
                            '(let ((x (f 1 2 3)))
                               (f x)))
                  (list (list 1 2 3)))
    (check-equal? (run-proc '(define (f x . xs) xs)
                            '(let ((x (f 1 2 3)))
                               (f x)))
                  '())
    (check-equal? (run-proc '(define (f x . xs)
                               (let ((ys xs))
                                 (if (empty? xs)
                                     x
                                     (apply f ys))))
                            '(let ((z 1))
                               (f 1 2 3)))
                  3)
    (check-equal? (run-proc '(define (f x . xs)
                               (let ((ys xs))
                                 (if (empty? xs)
                                     x
                                     (apply f ys))))
                            '(let ((z 1))
                               (f (f 1 2 3))))
                  3))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.
    )))

(define (make-io-test-suite name run/io-proc)
  (test-suite
   name

   ;; Evildoer examples
   (test-suite
    "Evildoer"
    (check-equal? (run/io-proc "" 7) (cons 7 ""))
    (check-equal? (run/io-proc "" '(write-byte 97)) (cons (void) "a"))
    (check-equal? (run/io-proc "a" '(read-byte)) (cons 97 ""))
    (check-equal? (run/io-proc "b" '(begin (write-byte 97) (read-byte))) (cons 98 "a"))
    (check-equal? (run/io-proc "" '(read-byte)) (cons eof ""))
    (check-equal? (run/io-proc "" '(eof-object? (read-byte))) (cons #t ""))
    (check-equal? (run/io-proc "a" '(eof-object? (read-byte))) (cons #f ""))
    (check-equal? (run/io-proc "" '(begin (write-byte 97) (write-byte 98))) (cons (void) "ab"))
    (check-equal? (run/io-proc "ab" '(peek-byte)) (cons 97 ""))
    (check-equal? (run/io-proc "ab" '(begin (peek-byte) (read-byte))) (cons 97 ""))
    (check-equal? (run/io-proc "†" '(read-byte)) (cons 226 "")))

   ;; Extort examples
   (test-suite
    "Extort"
    (check-equal? (run/io-proc "" '(write-byte #t)) (cons 'err "")))

   ;; Fraud examples
   (test-suite
    "Fraud"
    (check-equal? (run/io-proc "" '(let ([x 97]) (write-byte x))) (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(let ([x 97])
                                  (begin (write-byte x)
                                         x)))
                  (cons 97 "a"))
    (check-equal? (run/io-proc "b" '(let ([x 97]) (begin (read-byte) x)))
                  (cons 97 ""))
    (check-equal? (run/io-proc "b" '(let ([x 97]) (begin (peek-byte) x)))
                  (cons 97 "")))

   ;; Iniquity examples
   (test-suite
    "Iniquity"
    (check-equal? (run/io-proc ""
                               '(define (print-alphabet i)
                                  (if (zero? i)
                                      (void)
                                      (begin (write-byte (- 123 i))
                                             (print-alphabet (sub1 i)))))
                               '(print-alphabet 26))
                  (cons (void) "abcdefghijklmnopqrstuvwxyz"))
    (check-equal? (run/io-proc ""
                               '(define (f x)
                                  (write-byte x))
                               '(f 97))
                  (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(define (f x y)
                                  (write-byte x))
                               '(f 97 98))
                  (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(define (f x)
                                  (let ((y x))
                                    (write-byte y)))
                               '(f 97))
                  (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(define (f x y)
                                  (let ((y x))
                                    (write-byte y)))
                               '(f 97 98))
                  (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(define (f x)
                                  (write-byte x))
                               '(let ((z 97))
                                  (f z)))
                  (cons (void) "a"))
    (check-equal? (run/io-proc ""
                               '(define (f x y)
                                  (write-byte x))
                               '(let ((z 97))
                                  (f z 98)))
                  (cons (void) "a")))

   ;; Knock examples
   (test-suite
    "Knock"
    (check-equal? (run/io-proc ""
                               '(match (write-byte 97)
                                  [_ 1]))
                  (cons 1 "a")))

   ;; Loot examples
   (test-suite
    "Loot"
    (check-equal? (run/io-proc ""
                               '((begin (write-byte 97)
                                        (λ (x)
                                          (begin (write-byte x)
                                                 (write-byte 99))))
                                 98))
                  (cons (void) "abc")))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.
    )))
