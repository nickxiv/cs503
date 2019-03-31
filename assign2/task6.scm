(define (powerSet set)
    (define (helper source store)
        (cond 
            ((< (length set) source) store)
            (else
                (helper (+ source 1) (append store (sets-of-size source set)))
            )
        )
    )
    (helper 1 (list nil))
)

(define (sets-of-size size orig)
    (cond 
        ((= size (length orig)) (list orig))
        ((= size 0) (list nil))
        (else 
            (append (map 
                        (lambda (subset)
                            (cons (car orig) subset)) (sets-of-size (- size 1) (cdr orig)))
                    (sets-of-size size (cdr orig)))
        )
    )
)



(define (main)
    (setPort (open (getElement ScamArgs 1) 'read))
    (define env this)
    (setNilDisplay 'nil)
    (define (iter expr)
        (if (not (eof?)) 
            (begin (eval expr env) (iter (readExpr)))
        )
    )
    (iter (readExpr))    
)