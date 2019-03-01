(define (root-n-iter guess prev-guess x n)
    (lambda () #f)
    (cond 
        ((good-enough? guess prev-guess x n) guess)
        (else (root-n-iter (improve-guess guess x n) guess x n))
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

(define (good-enough? guess prev-guess x n)
    (< (cond 
            ((< prev-guess guess) (/ guess prev-guess))
            (else (/ prev-guess guess))
        ) 
        1.000001)
)

(define (root-n n)
    (lambda (x) 
        (root-n-iter 1.0 10.0 x n)
    )
)

(define (square x)
    (* x x)
)

(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define arg1 (readExpr))
    (define arg2 (readExpr))
    (println "((root-n " arg1 ") " arg2 ") is " (fmt "%.15f" ((root-n arg1) arg2)))
)