(define (sqrt-iter guess x n)
    (cond 
        ((good-enough? guess x n) guess)
        (else (sqrt-iter (improve-guess guess x n) x n))
    )    
)

(define (x^n x n)
    (cond ((= n 1) x)
    (else (* x (x^n x (- n 1)))))
)

(define (improve-guess guess x n)
    (/ (+ 
          (* (- n 1) guess)
          (/ x (x^n guess (- n 1)))
       ) 
        n)


)

(define (good-enough? guess x n)
    (< (abs (- (x^n guess n) x )) 0.00000000001)
)

(define (sqrt x n)
    (sqrt-iter 1.0 x n)
)

(define (square x)
    (* x x)
)

(define main
    (inspect (sqrt 8 3))    
)