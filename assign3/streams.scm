(define scar stream-car)
(define scdr stream-cdr)

;from class
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

;from class
(define (combine-streams op s t)
    (cons-stream 
        (op (scar s) (scar t))
        (combine-streams op (scdr s) (scdr t))
    )
)

;from class
(define (stream-map f s)
    (cons-stream
        (f (scar s))
        (stream-map f (scdr s)))
)

;from class
(define (partial-sums s)
    (cons-stream 
        (scar s)
        (combine-streams 
            + 
            (scdr s)
            (partial-sums s)
        )
    )
)

;from page 319 of SICP
(define (stream-ref s n)
    (if (= n 0)
        (scar s)
        (stream-ref (scdr s) (- n 1))
    )
)

