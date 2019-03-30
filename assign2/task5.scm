(define (tuple left right)
    this
)

(define (create n)
    (lambda (f)
        (lambda (iden)
            (define (helper source store)
                (cond 
                    ((= source 0) store)
                    (else
                        (helper (- source 1) (f store))
                    )
                )
            )
            (helper n iden)
        )
    )
)

(define (pred c-func)
    (define n (- ((c-func (lambda (x) (+ x 1))) 0) 1))
    (lambda (f)
        (lambda (iden)
            (define (helper source store)
                (cond 
                    ((= source 0) store)
                    (else
                        (helper (- source 1) (f store))
                    )
                )
            )
            (helper n iden)
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