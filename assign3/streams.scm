(define scar stream-car)
(define scdr stream-cdr)
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (combine-streams + ones wholes)))
(define wholes (cons-stream 0 (combine-streams + ones wholes)))

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
;from class
(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (merge 
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t))    
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

;from SICP
(define (stream-filter pred stream)
  (cond 
    ((nil? stream) nil
    ((pred (scar stream))
        (cons-stream 
            (scar stream)
            (stream-filter pred (scdr stream))
        )
    )
    (else 
        (stream-filter pred (scdr stream))))
    )
)


