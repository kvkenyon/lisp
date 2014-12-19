#lang racket

;;; -------
;;; Helpers
;;; -------

(define square 
  (lambda (a) 
    (* a a)))

(define double
  (lambda (a)
    (* 2 a)))

(define halve
  (lambda (a)
    (/ a 2)))

;;; Exercise 1.2

(/ (+ 5 4 
      (- 2 
         (- 3 
            (+ 6 
               (/ 4 5))))) 
   (* 3 
      (- 6 2) 
      (- 2 7)))

;;; Exercise 1.3
(define (sum-sq-larger-two a b c)
  (if (> a b)
      (if (> b c) (+ (* a a) (* b b))
          (+ (* a a) (* c c)))
      (if (> a c) (+ (* a a) (* b b))
          (+ (* b b) (* c c)))))

;;; Exercise 1.8

(define (cbrt x)
  (define (cbrt-itr guess x)
  (define (improve y x)
    (/ (+ (/ x (* y y)) (* 2 y))
       3))
  (define (good-enough? guess x)
    (< (abs (- (* guess guess guess) x)) .000001))
  (if (good-enough? guess x)
      guess
      (cbrt-itr (improve guess x) 
                x)))
  (cbrt-itr 1.0 x))

  
;;; Exercise 1.11
;;; f(n) = n , n <3
;;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) n>=3
;;; f(4) = f(3) + 2f(2) + 3f(1)
;;; f(4) = a + b + c
;;; f(5) = f(4) + 2f(3) + 3f(2)

;;; Recursive Procedure
(define (3fibr n)
  (if (<= n 3)
      n
      (+ (3fibr (- n 1))
         (* 2 (3fibr (- n 2)))
         (* 3 (3fibr (- n 3))))))

;;; Iterative Procedure [Tail-Recursive]
(define (3fibi n)
  (three-fib-iter 3 2 1 n))

(define (three-fib-iter a b c count)
         (if (= 1 count)
             c
             (three-fib-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

;;; Exercise 1.12
;;; Pascal's Triangle

(define (pascal r c) 
  (cond ((= c 0) 1)
        ((= r c) 1)
        ((< r 0) 1)
        ((< c 0) 1)
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))
        
;;; Exercise 1.16
;;; Fast-Exponentiation Iterative

(define (fast-expt-iter b n a)
  (cond ((= n 0) a) 
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))
      
(define (fast-expt b n)
  (fast-expt-iter b n 1))

;;; Exercise 1.17

(define (mult_r x y)
  (cond ((= y 0) 0)
        ((even? y) (mult_r (double x) (halve y)))
        (else (+ x (mult_r x (- y 1))))))
;;; Exercise 1.18

(define (mult-iter x y a)
  (cond ((= y 0) a)
        ((even? y) (mult-iter (double x) (halve y) a))
        (else (mult-iter x (- y 1) (+ a x)))))

(define mult_i (lambda (x y)
                 (mult-iter x y 0)))

;;; Exercise 1.19
(define (fib-test n)
  (fib-iter-test 1 0 n))

(define (fib-iter-test a b count)
  (if (= count 0)
      b
      (fib-iter-test (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))     ; compute p'
                   (+ (* 2 p q) (square q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; Exercise 1.22

(define (divides? a b)
  (= (remainder a b) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? n test) test)
        (else (find-divisor n (next test)))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (current-inexact-milliseconds) start-time)) (display " is not prime.")))

(define (report-prime elapsed-time)
  (display " is a prime (")
  (display elapsed-time)
  (display " milliseconds)."))

(define (search-for-primes a b)
  (cond ((> a b) (newline) (display "Done."))
        ((even? a) (search-for-primes (+ a 1) b))
        (else (timed-prime-test a) 
              (search-for-primes (+ a 2) b))))

;;; Exercise 1.23

(define (next n)
  (if (= n 2) 3 (+ 2 n)))

;;; Exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
  
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))


;;; Exercise 1.28
;;; Miller-Rabin Test (Primality)
(define (square-check a n)
  (if (and (not (or (= 1 a) (= a (- n 1))))
           (= 1 (remainder (square a) n))) 
      0
      (remainder (square a) n)))

(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check (miller-rabin-expmod base (/ exp 2) m)
                    m))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))
    
