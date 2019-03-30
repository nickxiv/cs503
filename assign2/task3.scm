(define (Stack) nil)

(define (push stack val)
    (cons val stack)
)

(define (pop stack) (cdr stack))

(define (ssize stack)
    (define (helper s curr)
        (cond 
            ((nil? s) curr)
            (else (helper (cdr s) (+ curr 1)))    
        )
    )
    (helper stack 0)
)

(define (speek stack) (car stack))

(define (Queue) (newQueue (Stack) (Stack)))     ;left stack for enqueue, right stack for dequeue
(define (newQueue left right) (cons left right))

(define (enqueue queue val)
    (cons (push (car queue) val) (cdr queue))
)

(define (dequeue queue)
    (cond 
        ((not (nil? (cdr queue))) (newQueue (car queue) (cddr queue)))
        (else 
            (newQueue nil (cdr (reverse (car queue))))
        )
    )
)

(define (reverse stack)
    (define (helper source store)
        (cond 
            ((nil? source) store)
            (else (helper (cdr source) (cons (car source) store)))
        )
    )
    (helper stack nil)
)

    
(define (qpeek queue) 
    (cond 
        ((not (nil? (cdr queue))) (speek (cdr queue)))
        (else 
            (qpeek (newQueue nil (reverse (car queue))))
        )
    )
)
(define (qsize queue) (+ (ssize (cdr queue)) (ssize (car queue))))
    

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