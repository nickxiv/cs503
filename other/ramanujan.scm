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

