(define (curry f @)
    (define nparams (length (get 'parameters f)))
    (define (helper source store list)          ;list = list of params
        (cond
            ((= store nparams) 
                (apply f list)
            )
            ((not (valid? source))
                (lambda (@)
                    (define new-params (append-lists @ list))
                    (cond 
                        ((= nparams (length new-params)) (apply f new-params))
                        (else (curry f new-params))
                    )
                )
            ) 
            (else
                (helper (cdr source) (+ store 1) (append-lists source list))    
            )
        )
    )
    (helper @ 0 nil)
)

(define (far-car list)
    (define (helper source store)
        (cond 
            ((nil? source) store)
            (else (helper (car source) source))
        )
    )
    (helper list nil)
)

(define (append-lists l1 l2)
    (define (helper source store)
        (cond 
            ((nil? source)
                store
            )
            (else (helper (cdr source) (cons (car source) store)))
        )
    )
    (helper l2 l1)
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