(include "streams.scm")

(define (mystery x)
    (cons-stream 1.0 (mystery-addends x 2.0))
)

(define (ps-mystery x)
    (partial-sums (mystery x))
)

(define (acc-mystery x)
    (euler-transform (ps-mystery x))
)

(define (super-mystery x)
    (accelerated-sequence euler-transform (ps-mystery x))
)

(define (make-tableau transform s)
    (cons-stream 
        s
        (make-tableau transform (transform s))    
    )
)

(define (accelerated-sequence transform s)
    (stream-map scar (make-tableau transform s))
)

(define (mystery-addends x n)
    (cons-stream
        (-
            (/
                (^ x n)
                (fact n)
            )
        )
        (stream-map - (mystery-addends x (+ n 2.0)))
    )
)

(define (symbolic-mystery)
    (println "(mystery x) is $\\cos x$")
)

(define (euler-transform s)
    (cons-stream
        (cond
            ((= 0.0 (+ (stream-ref s 0) (* -2 (stream-ref s 1)) (stream-ref s 2))) 
                (stream-ref s 2)
            )
            (else 
                (- 
                    (stream-ref s 2)
                    (/ 
                        (square (- (stream-ref s 2) (stream-ref s 1)))
                        (+ (stream-ref s 0) (* -2.0 (stream-ref s 1)) (stream-ref s 2))
                    )
                )
            )
        )
        (euler-transform (scdr s))
    )
)

(define (square x) (^ x 2.0))

(define (fact n)
    (define (helper source store)
        (cond 
            ((or (= source 1) (= source 0)) store)
            (else  
                (helper (- source 1) (* store source))
            )
        )
    )
    (helper n 1.0)
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