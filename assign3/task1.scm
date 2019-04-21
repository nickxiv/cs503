(define (scoping sym obj)
    (define (helper source)             ;source is an environment/object
        (cond 
            ((nil? source)
                'undefined
            )
            (else
                (define (checkAllHelper vars)   ;vars is the list of variable names in the curr env
                    (cond
                        ((nil? vars)
                            (helper (cadr (caddr source))) 
                        )
                        ((eq? (car vars) sym)
                            (if (eq? obj source)
                                'bound
                                'free
                            ) 
                        )
                        (else (checkAllHelper (cdr vars)))
                    )
                )
                (checkAllHelper (cadr source))
            )
        )
    )
    (helper obj)
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