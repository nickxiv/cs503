(define (let*->lambdas def)
    (define let*-body (caddr def))
    (cond 
        ((not (equal? (car let*-body) 'let*)) def)
        (else
            (define local-params (cadr let*-body))
            (list (car def) (cadr def) (car (make-lambdas local-params let*-body)))        
        )
    )
)

(define (reverse list)
    (define (helper source store)
        (cond 
            ((nil? source) store)
            (else (helper (cdr source) (cons (car source) store)))
        )
    )
    (helper list nil)
)

(define (cadar list)
    (car (cdr (car list)))
)
(define (make-lambdas locals let*-body)
    (define (helper source store)
        (cond
            ((nil? source) store)
            (else
                (helper 
                    (cdr source) 
                    ;(append-lists (list store (cadar source)) (list 'lambda (list (caar source))))
                    (list (list (append-lists store (list 'lambda (list (caar source)))) (cadar source)))
                )
            )
        )
    )
    (helper (reverse locals) (cddr let*-body))
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
    (helper (reverse l2) l1)
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