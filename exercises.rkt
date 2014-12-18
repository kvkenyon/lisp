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
        
        
        