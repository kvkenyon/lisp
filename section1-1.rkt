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


  
         
         
     
        