(define (accum op items base)
    (cond 
        ((null? items) base)
        (else
             (op (car items) (accum op (cdr items) base))
        )
    )
)

(define (accumL op items base)
    (cond
        ((null? items) base)
        ((null? (cdr items)) (car items))
        (else 
            (accumL op (cons (op (car items) (cadr items)) (cddr items)) base)
        )    
    )
)

(define items
    (append (map 
        (lambda (x) (list x (* x x)))
        (list 1 8 6)    
    ) nil)
)

(inspect (accum + (list 1 2 3 4) 0))
(inspect (accumL + (list 1 2 3 4) 0))