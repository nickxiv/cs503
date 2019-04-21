(include "streams.scm")

(define (cube x) (* x x x))
(define (weight i j) (+ (cube i) (cube j)))



(define (ramanujan)
    (define (helper source)
        (cond 
            (
                (= 
                    (weigh-pair (scar source)) 
                    (weigh-pair (scar (scdr source)))
                )
                (cons-stream 
                    (weigh-pair (scar source))
                    (helper (scdr source))
                )
            )
            (else 
                (helper (scdr source)))
            )
    )
    (helper (pairs wholes wholes))
)

(define (weigh-pair pair)
    (weight (car pair) (cdr pair))
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