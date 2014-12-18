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

  
         
         
     
        