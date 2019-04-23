(include "streams.scm")

(define (cube x) (^ x 3))

(define (merge-weighted s1 s2 weight) 
    (cond 
        ((stream-null? s1) s2) 
        ((stream-null? s2) s1) 
        (else 
            (cond 
                ((< (weight (stream-car s1)) (weight (stream-car s2))) 
                    (cons-stream (stream-car s1) 
                        (merge-weighted (stream-cdr s1) s2 weight)
                    )
                ) 
                ((> (weight (stream-car s1)) (weight (stream-car s2))) 
                    (cons-stream (stream-car s2) 
                        (merge-weighted s1 (stream-cdr s2) weight)
                    )
                ) 
                (else 
                    (cons-stream 
                        (stream-car s1) 
                        (cons-stream 
                            (stream-car s2) ;; must include both in case of ties! 
                            (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)
                        )
                    )
                )
            )
        )
    )
) 

(define (weighted-pairs s t weight) 
    (cons-stream 
        (list (stream-car s) (stream-car t)) 
        (merge-weighted 
            (merge-weighted 
                (stream-map 
                    (lambda (x) (list x (stream-car t))) 
                    (stream-cdr s)
                ) 
                (stream-map 
                    (lambda (x) (list (stream-car s) x)) 
                    (stream-cdr t)
                ) 
                weight
            ) 
            (weighted-pairs (stream-cdr s) (stream-cdr t) weight) 
            weight
        )
    )
)
    
(define (Ramanujan s)
    (inspect (cube-sum (scar s)))
    (inspect (cube-sum (scar (scdr s))))
    (inspect (cube-sum (scar (scdr (scdr s)))))
    (inspect (cube-sum (scar (scdr (scdr (scdr (scdr (scdr (scdr s)))))))))
    (cond 
        (= (cube-sum (scar s)) (cube-sum (scar (scdr s))) (cube-sum (scar (scdr (scdr s)))))  
                (cons-stream 
                (cube-sum (scar s)) 
                (Ramanujan (scdr (scdr (scdr s))))
            ) 
        (else 
            (Ramanujan (stream-cdr s))
        )
    )
)
 
 (define (cube-sum x) (+ (cube (car x)) (cube (cadr x))))   

(define (ramanujan) 
    (Ramanujan (weighted-pairs integers integers cube-sum))
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