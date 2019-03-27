(define (range a b step)
    (cond 
        ((>= a b) nil)
        (else (cons a (range (+ a step) b step)))    
    )
)

(define (for-loop vals f)
    (cond 
        ((nil? vals))
        (else 
            (f (car vals))
            (for-loop (cdr vals) f)
        )
    )
)


(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
    (define (iter expr)
        (if (not (eof?)) 
            (begin (eval expr env) (iter (readExpr)))
        )
    )
    (iter (readExpr))    
)