(define (nums a b step)
    (cond 
        ((>= a b) nil)
        (else (cons a (nums (+ a step) b step)))    
    )
)

(println (nums 0 10 5))