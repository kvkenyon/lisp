;;; Sum of integers from A --> B
(DEFINE (SUM-INT A B)
        (IF (> A B)
            0
            (+ A (SUM-INT (+ A 1) B))))

;;; Squares the integer A
(DEFINE (SQUARES A)
        (* A A))

;;; Sum of the squares of the integers from A --> B
(DEFINE (SUM-INT-SQUARES A B)
        (IF (> A B)
            0
            (+ (SQUARES A) (SUM-INT-SQUARES (+ A 1) B))))

;;; Too much redundancy in the above code, we want to
;;; isolate the idea of summation. And reuse that.

(DEFINE (SUM TERM A NEXT B)
        (IF (> A B)
            0
            (+ (TERM A) (SUM TERM (NEXT A) NEXT B))))

(DEFINE (SUM-INT2 A B)
        (SUM (LAMBDA (A) A) 
             A 
             (LAMBDA (A) (+ A 1)) 
             B))

(DEFINE (SUM-SQ A B)
        (SUM (LAMBDA (A) (SQUARES A)) 
             A 
             (LAMBDA (A) (+ A 1)) 
             B))

;;; Fixed-Points
;;; ============
;;; Heron's Method: Find fixed point by iterative method

(DEFINE (AVERAGE A B)
        (/ (+ A B) 2))

(DEFINE (CLOSE-ENUF? A B)
        (IF (< (- A B) .00000001)
            #T
            #F))
                

(DEFINE (FIXED-POINT F START)
        (DEFINE (ITER OLD NEW)
                (IF (CLOSE-ENUF? OLD NEW)
                    NEW
                    (ITER NEW (F NEW))))
        (ITER START (F START)))

(DEFINE AVERAGE-DAMP
        (LAMBDA (F)
                (LAMBDA (X) (AVERAGE (F X) X))))
                      
(DEFINE (MY-SQRT X)
        (FIXED-POINT
         (AVERAGE-DAMP (LAMBDA (Y) (/ X Y)))
         1))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (pow2 n)
  (if (= n 0)
      1
      (* 2 (pow2 (- n 1)))))

;;; Exercise 1.11
;;; f(n) = n , n <3
;;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) n>=3

;;; Recursive Procedure
(define (three-fib-scaled n)
  (if (< n 3)
      n
      (+ (three-fib-scaled (- n 1))
         (* 2 (three-fib-scaled (- n 2)))
         (* 3 (three-fib-scaled (- n 3))))))

;;; Iterative Procedure
(define (three-fib-scaled2 n)
  (three-fib-iter 1 2 3 n))

(define (three-fib-iter a b c count)
  