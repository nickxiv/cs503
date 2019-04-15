(define scar stream-car)
(define scdr stream-cdr)

(define (sdisplay n z)
    (print "(")
    (define (helper source store)
        (cond
            ((= source 0) (print "...)"))
            (else 
                (print (scar store) ",")
                (helper (- source 1) (scdr store))
            )
        )
    )
    (helper n z)    
)

(define (combine-streams op s t)
    (cons-stream 
        (op (scar s) (scar t))
        (combine-streams op (scdr s) (scdr t))
    )
)

(define (stream-map f s)
    (cons-stream
        (f (scar s))
        (stream-map f (scdr s)))
)


